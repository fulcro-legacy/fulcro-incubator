(ns fulcro.incubator.dynamic-routing
  (:require
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [taoensso.timbre :as log]))

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

(defn- apply-route* [state-map {:keys [router target]}]
  (let [router-class (-> router meta :component)
        target-class (-> target meta :component)]
    (-> state-map
      (assoc-in (conj router ::current-route) target)
      (update-in router dissoc ::pending-route)
      (prim/set-query* router-class {:query [::id {::current-route (prim/get-query target-class state-map)}]}))))

(defn target-ready*
  "Mutation helper: apply any pending route segment that has the `target` ident."
  [state-map target]
  (let [routers   (some-> state-map ::id vals)
        router-id (reduce (fn [_ r]
                            (when (= target (some-> r ::pending-route :target))
                              (reduced (::id r))))
                    nil
                    routers)]
    (if router-id
      (apply-route* state-map (get-in state-map [::id router-id ::pending-route]))
      state-map)))

(defmutation target-ready [{:keys [target]}]
  (action [{:keys [state]}]
    (swap! state target-ready* target))
  (refresh [_] [::current-route]))

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

(defmutation mark-route-pending
  "Mutation: Indicate that a given route is pending.

  router - The ident of the router, with metadata :component that is the class of the router.
  target - The ident of the target route, with metadata :component that is the class of the target."
  [{:keys [router target] :as params}]
  (action [{:keys [state]}]
    (swap! state assoc-in (conj router ::pending-route) params)))

(defn change-route
  "Trigger a route change.

  this - The component (or reconciler) that is causing the route change.
  new-route - A vector of URI components to pass to the router."
  [this new-route]
  (let [reconciler (prim/get-reconciler this)
        state-map  (-> reconciler prim/app-state deref)
        root       (prim/app-root reconciler)
        root-class (prim/react-type root)
        root-query (prim/get-query root-class state-map)
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
    (let [route-tx (atom [])]
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
                target-ident   (will-enter+ target reconciler params)]
            (if (immediate? target-ident)
              (swap! route-tx into `[(apply-route ~{:router (with-meta router-ident {:component component})
                                                    :target (with-meta target-ident {:component target})})])
              (swap! route-tx into `[(mark-route-pending ~{:router (with-meta router-ident {:component component})
                                                           :target (with-meta target-ident {:component target})})]))
            (when (seq remaining-path)
              (recur (ast-node-for-route target-ast remaining-path) remaining-path)))))
      ;; TASK: Cancel pending routes before potentially adding new ones...send user a route cancelled message.
      (prim/transact! reconciler (into [::current-route] @route-tx)))))


