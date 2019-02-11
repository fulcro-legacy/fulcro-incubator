(ns fulcro.incubator.dynamic-routing
  #?(:cljs (:require-macros [fulcro.incubator.dynamic-routing :refer [defsc-route-target]]))
  (:require
    [ghostwheel.core :refer [>fdef => ?]]
    #?(:clj [fulcro.incubator.defsc-extensions :as dext])
    [fulcro.incubator.ui-state-machines :as uism :refer [defstatemachine]]
    [fulcro.server-render :as ssr]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [clojure.spec.alpha :as s]
    #?(:clj [cljs.analyzer :as ana])
    [fulcro.util :as futil]))

;; STATIC protocol.
(defprotocol RouteTarget
  (route-segment [class] "Returns a vector that describes the sub-path that a given route represents. String elements represent
  explicit path elements, and keywords represent variable values (which are always pulled as strings).")
  (route-cancelled [class route-params] "Called if this target was in a deferred state and a different routing choice was made. Is given the same route parameters that were sent to `will-enter`.")
  (will-enter [class reconciler params] "Called before a route target is activated (if the route segment of interest has changed and the
  target of the result is this target).  MUST return (r/route-immediate ident) or (r/route-deferred ident) to indicate
  what ident should be used in app state to connect the router's join.  If deferred, the router must cause a call to
  the r/target-ready mutation (or use the target-ready* mutation helper) with a {:target ident} parameter to indicate
  that the route target is loaded and ready for display.

  `params` will be a map from any keywords found in `route-segment` to the string value of that path element.

  WARNING: This method MUST be side-effect free."))

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
(defn route-deferred [ident completion-fn] (with-meta ident {:immediate false
                                                             :fn        completion-fn}))
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

(defn current-route-class
  "Get the class of the component that is currently being routed to."
  [this]
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

(defn ast-node-for-live-router
  "Returns the AST node for a query that represents the closest \"live\" (on-screen) router

  ast - A query AST node

  Returns an AST node or nil if none is found."
  [reconciler {:keys [component children] :as ast-node}]
  (letfn [(live-router? [c] (and (router? c)
                              (boolean (prim/class->any reconciler c))))]
    (or
      (and (live-router? component) ast-node)
      (some #(and (live-router? (:component %)) %) children)
      (some #(ast-node-for-live-router reconciler %) children))))

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
  (-> env
    (uism/store :path-segment (uism/retrieve env :pending-path-segment))
    (uism/store :pending-path-segment [])
    (uism/apply-action target-ready* (uism/retrieve env :target))))

(defn fail-handler [env] env)

(defn route-handler [{::uism/keys [event-data] :as env}]
  (let [{:keys [router target error-timeout deferred-timeout path-segment] :or {error-timeout 5000 deferred-timeout 100}} event-data
        immediate? (immediate? target)]
    (-> (if immediate?
          (-> env
            (uism/store :path-segment path-segment)
            (uism/apply-action apply-route* event-data)
            (uism/activate :routed))
          (-> env
            (uism/store :pending-path-segment path-segment)
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

;; TODO: This algorithm is repeated in more than one place in slightly different forms...refactor it.
(defn proposed-new-path [this-or-reconciler relative-class-or-instance new-route]
  (let [reconciler (if (prim/reconciler? this-or-reconciler) this-or-reconciler (prim/get-reconciler this-or-reconciler))
        state-map  (-> reconciler prim/app-state deref)
        router     relative-class-or-instance
        root-query (prim/get-query router state-map)
        ast        (prim/query->ast root-query)
        root       (ast-node-for-route ast new-route)
        result     (atom [])]
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
              target-ident   (will-enter+ target reconciler params)]
          (when (vector? target-ident)
            (swap! result conj (vary-meta target-ident assoc :component target :params params)))
          (when (seq remaining-path)
            (recur (ast-node-for-route target-ast remaining-path) remaining-path)))))
    @result))

(defn signal-router-leaving
  "Tell active routers that they are about to leave the screen. Returns false if any of them deny the route change."
  [this-or-reconciler relative-class-or-instance new-route]
  #?(:clj
     true
     :cljs
     (let [new-path   (proposed-new-path this-or-reconciler relative-class-or-instance new-route)
           reconciler (if (prim/reconciler? this-or-reconciler) this-or-reconciler (prim/get-reconciler this-or-reconciler))
           state-map  (-> reconciler prim/app-state deref)
           router     relative-class-or-instance
           root-query (prim/get-query router state-map)
           ast        (prim/query->ast root-query)
           root       (ast-node-for-live-router reconciler ast)
           to-signal  (atom [])
           to-cancel  (atom [])
           _          (loop [{:keys [component] :as node} root new-path-remaining new-path]
                        (when (and component (router? component))
                          (let [new-target    (first new-path-remaining)
                                router-ident  (prim/get-ident component {})
                                active-target (get-in state-map (conj router-ident ::current-route))
                                {:keys [target]} (get-in state-map (conj router-ident ::pending-route))
                                next-router   (some #(ast-node-for-live-router reconciler %) (:children node))]
                            (when (futil/ident? target)
                              (swap! to-cancel conj target))
                            (when (and (not= new-target active-target) (vector? active-target))
                              (when-let [c (prim/ref->any reconciler active-target)]
                                (swap! to-signal conj c)))
                            (when next-router
                              (recur next-router (rest new-path-remaining))))))
           components (reverse @to-signal)
           result     (atom true)]
       (doseq [c components]
         (swap! result #(and % (will-leave c (prim/props c)))))
       (when @result
         (doseq [t @to-cancel]
           (let [{:keys [component params]} (some-> t meta)]
             (route-cancelled component params))))
       @result)))

(defn change-route-relative
  "Change the route, starting at the given Fulcro class or instance (scanning for the first router from there).  `new-route` is a vector
  of string components to pass through to the nearest child router as the new path. The first argument is any live component
  or the reconciler.  The `timeouts` are as in `change-route`.
  It is safe to call this from within a mutation."
  ([this-or-reconciler relative-class-or-instance new-route]
   (change-route-relative this-or-reconciler relative-class-or-instance new-route {}))
  ([this-or-reconciler relative-class-or-instance new-route timeouts]
   (if-let [ok? (signal-router-leaving this-or-reconciler relative-class-or-instance new-route)]
     (uism/defer
       #(let [reconciler (if (prim/reconciler? this-or-reconciler) this-or-reconciler (prim/get-reconciler this-or-reconciler))
              state-map  (-> reconciler prim/app-state deref)
              router     relative-class-or-instance
              root-query (prim/get-query router state-map)
              ast        (prim/query->ast root-query)
              root       (ast-node-for-route ast new-route)]
          (loop [{:keys [component]} root path new-route]
            (when (and component (router? component))
              (let [{:keys [target matching-prefix]} (route-target component path)
                    target-ast        (some-> target (prim/get-query state-map) prim/query->ast)
                    prefix-length     (count matching-prefix)
                    remaining-path    (vec (drop prefix-length path))
                    segment           (route-segment+ target)
                    params            (reduce
                                        (fn [p [k v]] (if (keyword? k) (assoc p k v) p))
                                        {}
                                        (map (fn [a b] [a b]) segment matching-prefix))
                    router-ident      (prim/get-ident component {})
                    router-id         (-> router-ident second)
                    target-ident      (will-enter+ target reconciler params)
                    completing-action (or (some-> target-ident meta :fn) identity)
                    event-data        (merge
                                        {:error-timeout 5000 :deferred-timeout 100}
                                        timeouts
                                        {:path-segment matching-prefix
                                         :router       (vary-meta router-ident assoc :component component)
                                         :target       (vary-meta target-ident assoc :component target :params params)})]
                (completing-action)
                (if-not (uism/get-active-state reconciler router-id)
                  (uism/begin! this-or-reconciler RouterStateMachine router-id
                    {:router (uism/with-actor-class router-ident component)}
                    event-data)
                  (uism/trigger! reconciler router-id :route! event-data))
                (when (seq remaining-path)
                  (recur (ast-node-for-route target-ast remaining-path) remaining-path))))))))))

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
   (let [reconciler (if (prim/reconciler? this) this (prim/get-reconciler this))
         root       (prim/app-root reconciler)]
     (change-route-relative reconciler root new-route timeouts))))

(defn current-route
  "Returns the current active route, starting from the relative Fulcro class or instance."
  [this-or-reconciler relative-class-or-instance]
  (let [reconciler (if (prim/reconciler? this-or-reconciler) this-or-reconciler (prim/get-reconciler this-or-reconciler))
        state-map  (-> reconciler prim/app-state deref)
        router     relative-class-or-instance
        root-query (prim/get-query router state-map)
        ast        (prim/query->ast root-query)
        root       (ast-node-for-live-router reconciler ast)
        result     (atom [])]
    (loop [{:keys [component] :as node} root]
      (when (and component (router? component))
        (let [router-ident (prim/get-ident component {})
              router-id    (-> router-ident second)
              sm-env       (uism/state-machine-env state-map nil router-id :none {})
              path-segment (uism/retrieve sm-env :path-segment)
              next-router  (some #(ast-node-for-live-router reconciler %) (:children node))]
          (when (seq path-segment)
            (swap! result into path-segment))
          (when next-router
            (recur next-router)))))
    @result))

#?(:clj
   (defn compile-error [env form message]
     (throw (ana/error (merge env (some-> form meta)) message {}))))

#?(:clj (s/def ::router-targets (s/coll-of symbol? :type vector?)))
#?(:clj (s/def ::initial-ui list?))
#?(:clj (s/def ::loading-ui list?))
#?(:clj (s/def ::failed-ui list?))
#?(:clj (s/def ::defrouter-options (s/keys :req-un [::router-targets] :opt-un [::initial-ui ::loading-ui ::failed-ui])))

#?(:clj
   (defn defrouter* [env router-sym arglist options body]
     (when-not (and (vector? arglist) (= 2 (count arglist)))
       (compile-error env options "defrouter argument list must have an entry for this and props."))
     (when-not (map? options)
       (compile-error env options "defrouter requires a literal map of options."))
     (when-not (s/valid? ::defrouter-options options)
       (compile-error env options (str "defrouter options are invalid: " (s/explain-str ::defrouter-options options))))
     (let [{:keys [router-targets]} options
           id                     (-> router-sym name keyword)
           query                  (into [::id
                                         [::uism/asm-id id]
                                         {::current-route `(prim/get-query ~(first router-targets))}]
                                    (map-indexed
                                      (fn [idx s] {(keyword (str "alt" idx)) `(prim/get-query ~s)})
                                      (rest router-targets)))
           initial-state-map      (into {::id            id
                                         ::current-route `(prim/get-initial-state ~(first router-targets) ~'params)}
                                    (map-indexed
                                      (fn [idx s] [(keyword (str "alt" idx)) `(prim/get-initial-state ~s {})])
                                      (rest router-targets)))
           ident-method           (apply list `(fn [] [::id ~id]))
           get-targets-method     (apply list `(~'get-targets [~'c] ~(set router-targets)))
           initial-state-lambda   (apply list `(fn [~'params] ~initial-state-map))
           states-to-render-route (if (seq body)
                                    #{:routed :deferred}
                                    `(constantly true))
           render-cases           (apply list `(if (~states-to-render-route ~'current-state)
                                                 (if-let [~'class (fulcro.incubator.dynamic-routing/current-route-class ~'this)]
                                                   (let [~'factory (prim/factory ~'class)]
                                                     (~'factory ~'current-route)))
                                                 (let [~(first arglist) ~'this
                                                       ~(second arglist) {:pending-path-segment ~'pending-path-segment
                                                                          :current-state        ~'current-state}]
                                                   ~@body)))
           options                (merge (dissoc options :router-targets) `{:query         ~query
                                                                            :ident         ~ident-method
                                                                            :protocols     [~'static fulcro.incubator.dynamic-routing/Router
                                                                                            ~get-targets-method]
                                                                            :initial-state ~initial-state-lambda})]
       `(prim/defsc ~router-sym [~'this {::keys [~'id ~'current-route] :as ~'props}]
          ~options
          (let [~'current-state (uism/get-active-state ~'this ~id)
                ~'state-map (prim/component->state-map ~'this)
                ~'sm-env (uism/state-machine-env ~'state-map nil ~id :fake {})
                ~'pending-path-segment (uism/retrieve ~'sm-env :pending-path-segment)]
            ~render-cases)))))

#?(:clj
   (defmacro defrouter
     "Define a router.

     The arglist is `[this props]`, which are just like defsc. The props will contains :current-state and :pending-path-segment.

     The options are:

     `:router-targets` - (REQUIRED) A *vector* of ui components that are router targets. The first one is considered the \"default\".
     Other defsc options - (LIMITED) You may not specify query/initial-state/protocols/ident, but you can define things like react
     lifecycle methods. See defsc.

     The optional body, if defined, will *only* be used if the router is in a pending (deferred) or initial state (:initial,
     :pending, or :failed), otherwise the actual route target will be rendered.
     "
     [router-sym arglist options & body]
     (defrouter* &env router-sym arglist options body)))

#?(:clj
   (s/fdef defrouter
     :args (s/cat :sym symbol? :arglist vector? :options map? :body (s/* any?))))

#?(:clj
   (dext/defextended-defsc defsc-route-target [[`RouteLifecycle false] [`RouteTarget true]]))

(defn ssr-initial-state
  "(ALPHA) A helper to get initial state database for SSR.

  Returns:

  ```
  {:db normalized-db
   :props props-to-render}
  ```

  IMPORTANT NOTES:

  - `will-enter` for the routes will *not* get a reconciler (since there
  isn't one).  Be sure your routers will tolerate a nil reconciler.
  - This has not been well-tested.  It is known to render correct HTML in simple cases, but the initial state
  may not actually be correct for the starting app with respect to the routers.
  "
  [app-root-class root-router-class route-path]
  (let [initial-tree (prim/get-initial-state app-root-class {})
        initial-db   (ssr/build-initial-state initial-tree app-root-class)
        router-ident (prim/get-ident root-router-class {})
        instance-id  (second router-ident)
        {:keys [target matching-prefix]} (route-target root-router-class route-path)
        target-ident (will-enter+ target nil nil)           ; Target in this example needs neither
        params       {::uism/asm-id           instance-id
                      ::uism/state-machine-id (::state-machine-id RouterStateMachine)
                      ::uism/event-data       (merge
                                                {:path-segment matching-prefix
                                                 :router       (vary-meta router-ident assoc
                                                                 :component root-router-class)
                                                 :target       (vary-meta target-ident assoc
                                                                 :component target)})
                      ::uism/actor->ident     {:router (uism/with-actor-class router-ident root-router-class)}}
        initial-db   (assoc-in initial-db [::uism/asm-id :RootRouter] (uism/new-asm params))]
    {:db    initial-db
     :props (prim/db->tree (prim/get-query app-root-class initial-db)
              initial-db initial-db)}))
