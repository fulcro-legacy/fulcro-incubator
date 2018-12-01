(ns fulcro.incubator.dynamic-routing
  #?(:cljs (:require-macros fulcro.incubator.dynamic-routing))
  (:require
    [ghostwheel.core :as g :refer [>fdef => ?]]
    [fulcro.incubator.ui-state-machines :as uism :refer [defstatemachine]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [taoensso.timbre :as log]
    [clojure.spec.alpha :as s]
    #?(:clj [cljs.analyzer :as ana])))

;; STATIC protocol.
(defprotocol RouteTarget
  (route-segment [class] "Returns a vector that describes the sub-path that a given route represents. String elements represent
  explicit path elements, and keywords represent variable values (which are always pulled as strings).")
  (will-enter [class reconciler params] "Called before a route target is activated (if the route segment of interest has changed and the
  target of the result is this target).  MUST return (r/route-immediate ident) or (r/route-deferred ident) to indicate
  what ident should be used in app state to connect the router's join.  If deferred, the router must cause a call to
  the r/target-ready mutation (or use the target-ready* mutation helper) with a {:target ident} parameter to indicate
  that the route target is loaded and ready for display.

  `params` will be a map from any keywords found in `route-segment` to the string value of that path element."))

(defn route-segment+
  "Universal CLJC version of route-segment.  Don't use the protocol method in CLJ."
  [class]
  #?(:clj  (when-let [route-segment (-> class meta :route-segment)]
             (route-segment class))
     :cljs (when (implements? RouteTarget class)
             (route-segment class))))

(defn will-enter+
  "Universal CLJC version of will-enter.  Don't use the protocol method in CLJ."
  [class rec params]
  #?(:clj  (when-let [will-enter (-> class meta :will-enter)]
             (will-enter class rec params))
     :cljs (when (implements? RouteTarget class)
             (will-enter class rec params))))

(defn route-target? [component]
  #?(:clj  (if (fn? component)
             (some? (-> component meta :route-segment))
             (let [class (cond-> component (prim/component? component) class)]
               (extends? RouteTarget class)))
     :cljs (implements? RouteTarget component)))

;; NON-static protocol for interacting as a route target
(defprotocol RouteLifecycle
  (will-leave [this props] "If a route target implements this non-static protocol, then `will-leave` will be called with
  the current component and props. If it returns `true` then the routing operation will continue.  If it returns `false`
  then whatever new route was requested will be completely abandoned.  It is the responsibility of this method to give
  UI feedback as to why the route change was aborted."))

(defn route-lifecycle? [component]
  #?(:clj  (if (fn? component)
             (some? (-> component meta :will-leave))
             (let [class (cond-> component (prim/component? component) class)]
               (extends? RouteLifecycle class)))
     :cljs (implements? RouteLifecycle component)))

(defprotocol Router
  (get-targets [_] "Returns a set of classes to which this router routes."))

(defn get-targets+
  "CLJC version of get-targets"
  [class]
  #?(:clj  (when-let [get-targets (-> class meta :get-targets)]
             (get-targets class))
     :cljs (when (implements? Router class)
             (get-targets class))))


(defn route-immediate [ident] (with-meta ident {:immediate true}))
(defn route-deferred [ident] (with-meta ident {:immediate false}))
(defn immediate? [ident] (some-> ident meta :immediate))

(defn- apply-route* [state-map {:keys [router target] :as params}]
  (let [router-class (-> router meta :component)
        router-id    (second router)
        target-class (-> target meta :component)]
    (-> state-map
      (assoc-in (conj router ::current-route) target)
      (update-in router dissoc ::pending-route)
      (prim/set-query* router-class {:query [::id [::uism/asm-id router-id] {::current-route (prim/get-query target-class state-map)}]}))))

(defn router-for-pending-target [state-map target]
  (let [routers   (some-> state-map ::id vals)
        router-id (reduce (fn [_ r]
                            (when (= target (some-> r ::pending-route :target))
                              (reduced (::id r))))
                    nil
                    routers)]
    router-id))

(defn target-ready*
  "Mutation helper: apply any pending route segment that has the `target` ident."
  [state-map target]
  (let [router-id (router-for-pending-target state-map target)]
    (if router-id
      (apply-route* state-map (get-in state-map [::id router-id ::pending-route]))
      state-map)))

(defmutation target-ready [{:keys [target]}]
  (action [{:keys [reconciler state]}]
    (when-let [router-id (router-for-pending-target @state target)]
      (uism/trigger! reconciler router-id :ready!)))
  (refresh [_] [:route]))

(defn router? [component]
  (and component
    #?(:clj  (if (fn? component)
               (some? (-> component meta :get-targets))
               (let [class (cond-> component (prim/component? component) class)]
                 (extends? Router class)))
       :cljs (implements? Router component))))

(defn matching-prefix
  "Returns the elements of actual-path that match the route-segment definition."
  [route-segment actual-path]
  (let [matching-segment (reduce
                           (fn [result [expected actual]]
                             (cond
                               (and (string? expected) (= expected actual))
                               (conj result actual)

                               (and (keyword? expected) (seq (str actual)))
                               (conj result (str actual))

                               :otherwise result))
                           []
                           (map (fn [a b] [a b]) route-segment actual-path))]
    (when (= (count matching-segment) (count route-segment))
      matching-segment)))

(defn- current-route-class [this]
  (let [state-map (prim/component->state-map this)
        class     (some->> (prim/get-query this state-map) prim/query->ast :children
                    (filter #(= ::current-route (:key %))) first :component)]
    class))

(defn route-target
  "Given a router class and a path segment, returns the class of the router-class that is the target of the given URI path,
  which is a vector of (string) URI components.

  Returns nil if there is no target that accepts the path, or a map containing:

  {:target class
   :matching-prefix prefix}

  where `class` is the component class that accepts the path (the target, NOT the router), and `matching-prefix` is the
  portion of the path that is accepted by that class.
  "
  [router-class path]
  (when (and router-class (router? router-class))
    (let [targets (get-targets+ router-class)]
      (reduce (fn [result target-class]
                (let [prefix (and target-class (route-target? target-class)
                               (some-> target-class (route-segment+) (matching-prefix path)))]
                  (if (seq prefix)
                    (reduced {:matching-prefix prefix
                              :target          target-class})
                    result))) nil targets))))

(defn accepts-route?
  "Returns true if the given component is a router that manages a route target that will accept the given path."
  [component path]
  (boolean (route-target component path)))

(defn ast-node-for-route
  "Returns the AST node for a query that represents the router that has a target that can accept the given path. This is a breadth-first
  search.

  ast - A query AST node
  path - A vector of the current URI segments.

  Returns an AST node or nil if none is found."
  [{:keys [component children] :as ast-node} path]
  (or
    (and (accepts-route? component path) ast-node)
    (some #(and (accepts-route? (:component %) path) %) children)
    (some #(ast-node-for-route % path) children)))

(defmutation apply-route
  "Mutation: Indicate that a given route is ready and should show the result.

  router - The ident of the router, with metadata :component that is the class of the router.
  target - The ident of the target route, with metadata :component that is the class of the target."
  [{:keys [router target] :as params}]
  (action [{:keys [state]}]
    (swap! state apply-route* params)))

(defn mark-route-pending* [state-map {:keys [router target] :as params}]
  (assoc-in state-map (conj router ::pending-route) params))

(defn ready-handler [env]
  (uism/apply-action env target-ready* (uism/retrieve env :target)))

(defn fail-handler [env] env)

(defn route-handler [{::uism/keys [event-data] :as env}]
  (let [{:keys [router target error-timeout deferred-timeout] :or {error-timeout 5000 deferred-timeout 100}} event-data
        immediate? (immediate? target)]
    (-> (if immediate?
          (-> env
            (uism/apply-action apply-route* event-data)
            (uism/activate :routed))
          (-> env
            (uism/apply-action mark-route-pending* event-data)
            (uism/set-timeout :error-timer :timeout! {} error-timeout #{:ready! :route!})
            (uism/set-timeout :delay-timer :waiting! {} deferred-timeout #{:ready! :route!})
            (uism/activate :deferred)))
      (uism/store :target target))))

(defstatemachine RouterStateMachine
  {::uism/actors
   #{:router}

   ::uism/aliases
   {:current-route [:router ::current-route]
    :state         [:router ::current-state]}

   ::uism/states
   {:initial  {::uism/handler route-handler}

    :deferred {::uism/events
               {:waiting! {::uism/target-state :pending}
                :route!   {::uism/handler route-handler}
                :ready!   {::uism/target-state :routed
                           ::uism/handler      ready-handler}
                :timeout! {::uism/target-state :failed
                           ::uism/handler      fail-handler}}}

    :pending  {::uism/events
               {:waiting! {::uism/target-state :pending}
                :route!   {::uism/handler route-handler}
                :ready!   {::uism/target-state :routed
                           ::uism/handler      ready-handler}
                :timeout! {::uism/target-state :failed
                           ::uism/handler      fail-handler}}}

    ;; failed may potentially resolve (just very late), so it must accept ready! events
    :failed   {::uism/events
               {:route! {::uism/handler route-handler}
                :ready! {::uism/target-state :routed
                         ::uism/handler      ready-handler}}}

    :routed   {::uism/handler route-handler}}})


(defn change-route
  "Trigger a route change.

  this - The component (or reconciler) that is causing the route change.
  new-route - A vector of URI components to pass to the router.
  timeouts - A map of timeouts that affect UI during deferred routes: {:error-timeout ms :deferred-timeout ms}

  The error timeout is how long to wait  (default 5000ms) before showing the error-ui of a route (which must be defined on the
  router that is having problems).  The deferred-timeout (default 100ms) is how long to wait before showing the loading-ui of
  a deferred router (to prevent flicker).
  "
  ([this new-route]
   (change-route this new-route {}))
  ([this new-route timeouts]
   (uism/defer
     #(let [reconciler (if (prim/reconciler? this) this (prim/get-reconciler this))
            state-map  (-> reconciler prim/app-state deref)
            root       (prim/app-root reconciler)
            root-query (prim/get-query root state-map)
            ast        (prim/query->ast root-query)
            root       (ast-node-for-route ast new-route)]
        ;; breadth-first search for routers in the query. algorithm is as follows (app UI root starts out as "root"):
        ;; 1. Breadth-first search for a router that can consume prefix off of new-route that router is now "root".
        ;; 3. Do the routing step for "root", which will either be immediate or deferred:
        ;;   - Immediate: Update ident, remove elements from new-route that were consumed, and resume step 1 starting from current "root"
        ;;   - Deferred: We'll know the ident and class (since we get the ident from the target class of the router). Do NO actual state change,
        ;;     but the remainder of the alg can continue as in "Immediate".
        ;; 4. This continues until the new-route elements are consumed, or we reach the end of the query.
        ;; Overall this is a static analysis of possible routes, so the current query doesn't matter since we're "skipping"
        ;; over the dynamic queries of the router nodes.
        (loop [{:keys [component]} root path new-route]
          (when (and component (router? component))
            (let [{:keys [target matching-prefix]} (route-target component path)
                  target-ast     (some-> target (prim/get-query state-map) prim/query->ast)
                  prefix-length  (count matching-prefix)
                  remaining-path (vec (drop prefix-length path))
                  segment        (route-segment+ target)
                  params         (reduce
                                   (fn [p [k v]] (if (keyword? k) (assoc p k v) p))
                                   {}
                                   (map (fn [a b] [a b]) segment matching-prefix))
                  router-ident   (prim/get-ident component {})
                  router-id      (-> router-ident second)
                  target-ident   (will-enter+ target reconciler params)
                  event-data     (merge
                                   {:error-timeout 5000 :deferred-timeout 100}
                                   timeouts
                                   {:router (vary-meta router-ident assoc :component component)
                                    :target (vary-meta target-ident assoc :component target)})]
              (if-not (uism/get-active-state reconciler router-id)
                (uism/begin! this RouterStateMachine router-id
                  {:router (uism/with-actor-class router-ident component)}
                  event-data)
                (uism/trigger! reconciler router-id :route! event-data))
              (when (seq remaining-path)
                (recur (ast-node-for-route target-ast remaining-path) remaining-path)))))))))

(comment
  (prim/defsc SettingsPaneRouter [this {:fulcro.incubator.dynamic-routing/keys [id current-route] :as props}]
    {:query         (fn [] [:fulcro.incubator.dynamic-routing/id [:fulcro.incubator.ui-state-machines/asm-id :SettingsPaneRouter]
                            {:fulcro.incubator.dynamic-routing/current-route (prim/get-query Pane1)}
                            {:alt0 (prim/get-query Pane2)}])
     :ident         (fn [] [:fulcro.incubator.dynamic-routing/id :SettingsPaneRouter])
     :protocols     [static dr/Router
                     (get-targets [_] #{Pane1 Pane2})]
     :initial-state (fn [params]
                      {:fulcro.incubator.dynamic-routing/id            :SettingsPaneRouter
                       :fulcro.incubator.dynamic-routing/current-route (prim/get-initial-state Pane1 params)
                       :alt0                                           (prim/get-initial-state Pane1 {})})}
    (let [current-state (get-in props [[:fulcro.incubator.ui-state-machines/asm-id :SettingsPaneRouter]
                                       :fulcro.incubator.ui-state-machines/active-state])]
      (case current-state
        :initial (dom/div "Just Started.")
        :pending (dom/div "LOADING...")
        :failed (dom/div "Screwed.")
        (:deferred :routed) (if-let [class (dr/current-route-class this)]
                              (let [factory (prim/factory class)]
                                (factory current-route)))
        nil))))

#?(:clj
   (defn compile-error [env form message]
     (throw (ana/error (merge env (some-> form meta)) message {}))))

#?(:clj (s/def ::router-targets (s/coll-of symbol? :type vector?)))
#?(:clj (s/def ::initial-ui list?))
#?(:clj (s/def ::loading-ui list?))
#?(:clj (s/def ::failed-ui list?))
#?(:clj (s/def ::defrouter-options (s/keys :req-un [::router-targets] :opt-un [::initial-ui ::loading-ui ::failed-ui])))

#?(:clj
   (defn defrouter* [env router-sym options]
     (when-not (map? options)
       (compile-error env options "defrouter requires a literal map of options."))
     (when-not (s/valid? ::defrouter-options options)
       (compile-error env options (str "defrouter options are invalid: " (s/explain-str ::defrouter-options options))))
     (let [{:keys [router-targets initial-ui loading-ui failed-ui]} options
           id                   (-> router-sym name gensym name keyword)
           query                (into [::id
                                       [::uism/asm-id id]
                                       {::current-route `(prim/get-query ~(first router-targets))}]
                                  (map-indexed
                                    (fn [idx s] {(keyword (str "alt" idx)) `(prim/get-query ~s)})
                                    (rest router-targets)))
           initial-state-map    (into {::id            id
                                       ::current-route `(prim/get-initial-state ~(first router-targets) ~'params)}
                                  (map-indexed
                                    (fn [idx s] [(keyword (str "alt" idx)) `(prim/get-initial-state ~s {})])
                                    (rest router-targets)))
           ident-method         (apply list `(fn [] [::id ~id]))
           get-targets-method   (apply list `(~'get-targets [~'c] ~(set router-targets)))
           initial-state-lambda (apply list `(fn [~'params] ~initial-state-map))
           default-cases        (cond-> [:routed :deferred]
                                  (nil? initial-ui) (conj :initial)
                                  (nil? loading-ui) (conj :pending)
                                  (nil? failed-ui) (conj :failed))
           render-cases         (apply list (cond-> `[case ~'current-state
                                                      ~(apply list default-cases) (if-let [~'class (fulcro.incubator.dynamic-routing/current-route-class ~'this)]
                                                                                    (let [~'factory (prim/factory ~'class)]
                                                                                      (~'factory ~'current-route)))]
                                              initial-ui (into [:initial initial-ui])
                                              loading-ui (into [:pending loading-ui])
                                              failed-ui (into [:failed failed-ui])
                                              initial-ui (into [initial-ui]) ; default, if defined
                                              (not initial-ui) (into [nil])))]
       `(prim/defsc ~router-sym [~'this {::keys [~'id ~'current-route] :as ~'props}]
          {:query         ~query
           :ident         ~ident-method
           :protocols     [~'static fulcro.incubator.dynamic-routing/Router
                           ~get-targets-method]
           :initial-state ~initial-state-lambda}
          (let [~'current-state (get-in ~'props [[:fulcro.incubator.ui-state-machines/asm-id ~id]
                                                 :fulcro.incubator.ui-state-machines/active-state])]
            ~render-cases)))))

#?(:clj
   (defmacro defrouter
     "Define a router. The options are:

     `:router-targets` - (REQUIRED) A *vector* of ui components that are router targets. The first one is considered the \"default\".
     `:initial-ui` - (optional) Dom to render when the router has yet to route.
     `:loading-ui` - (optional) Dom to render when there is a pending route (after :deferred-timeout millis).
     `:failed-ui` - (optional) Dom to render when the route has failed to resolve after :error-timeout millis.

     If any ui parameter is missing, then the default will be whatever the current state of the router is/was showing.
     "
     [router-sym options]
     (defrouter* &env router-sym options)))

#?(:clj
   (s/fdef defrouter
     :args (s/cat :sym symbol? :options ::defrouter-options)))
