(ns fulcro.incubator.routing-ws
  (:require
    [nubank.workspaces.core :refer [deftest]]
    [fulcro-spec.core :refer [assertions component]]
    [nubank.workspaces.core :as ws]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.client.routing :as fr :refer [defrouter]]
    [fulcro.server :as server]
    [fulcro-spec.core :refer [specification assertions]]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.client.dom :as dom]
    [nubank.workspaces.model :as wsm]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [fulcro.client.mutations :as m]
    [taoensso.timbre :as log]
    [fulcro.client.data-fetch :as df]))

(defprotocol Routing
  (route-entered [this remaining-path] "Notifies the component that the current route has caused it to become visible.
  remaining-path is a vector of URI components that were not interpreted by parents. Each router is responsible for passing this on, so composition can work,
  but that can be wrapped in an automatic macro I think"))

(defn- find-next-router* [ast-node]
  (let [c (:component ast-node)]
    (if (implements? Routing c)
      c
      (some #(find-next-router* %) (:children ast-node)))))

(defn find-next-router
  "Finds the next router available on the given query (not including the top component on the query itself), and
  returns it's component class. Returns nil if none are found."
  [query]
  (let [nodes (:children (prim/query->ast query))]
    (some #(find-next-router* %) nodes)))

;; SEE TopRouter...this is just a placeholder, and doesn't show everything is should do
(defsc DistantRouter [this props]
  {:query                ['*]
   :ident                (fn [] [:distant-router 1])
   :componentWillUnmount (fn [] (js/console.log "Distant router leaving the screen"))
   :initial-state        {}
   :protocols            [static
                          Routing
                          (route-entered [this path]
                            (js/console.log "Distant router Entered with " path))]}
  (dom/div "DISTANT ROUTER CONTENT"))

(def ui-distant-router (prim/factory DistantRouter))

;; SEE TopRouter...this is just a placeholder, and doesn't show everything is should do
(defsc OtherRouter [this props]
  {:query                ['*]
   :ident                (fn [] [:other-router 1])
   :componentWillUnmount (fn [] (js/console.log "Other router leaving the screen"))
   :initial-state        {}
   :protocols            [static
                          Routing
                          (route-entered [this path]
                            (js/console.log "Other router Entered with " path))]}
  (dom/div
    "OTHER ROUTER CONTENT"))

(def ui-other-router (prim/factory OtherRouter))

(defsc Y [this {:keys [prop]}]
  {:query         [:prop]
   :ident         (fn [] [:Y 1])
   :initial-state {:prop 22}}
  (dom/div (str "Y" prop)))

(def ui-y (prim/factory Y))

(defsc X [this {:keys [left right] :as props}]
  {:query         [{:left (prim/get-query Y)}
                   {:right (prim/get-query DistantRouter)}]
   :ident         (fn [] [:X 1])
   :initial-state {:left {} :right {}}}
  (dom/div
    (dom/p "X Content")
    (ui-y left)
    (ui-distant-router right)))

(def ui-x (prim/factory X {:keyfn :db/id}))

;; THIS IS THE GENERAL PATTERN ALL ROUTERS WOULD HAVE
(defsc TopRouter [this {:keys [child]}]
  {:query          [{:child (prim/get-query X)}]
   :initial-state  {:child {}}
   :ident          (fn [] [:top-router 1])
   :initLocalState (fn [] {:factory (prim/factory X)})
   :protocols      [static
                    Routing
                    (route-entered [this path]
                      ;; This is the main logic that would really go in EACH router.  A router ONLY EVER has one child,
                      ;; which need NOT be a router itself.  You can point a router at anything.   There MUST only ever
                      ;; be AT MOST ONE router directly reachable through that child (without going *through* another router.
                      (let [state-map             (prim/component->state-map this)
                            ;; Need to get the *live* query, since we're using dynamic queries
                            q                     (prim/get-query this state-map)
                            ;; In order to render, we need to use the correct factory for our ONE child.
                            ;; I'm putting in state so we don't have to recalculate it on every render.
                            immediate-child-class (:component (prim/query->ast1 q))
                            factory               (prim/factory immediate-child-class)
                            ;; Then we look for that ONE reachable router
                            child-router          (find-next-router q)]
                        (prim/set-state! this {:factory factory})
                        (js/console.log "Entered TopRouter with " path " whose immediate child is a " immediate-child-class)
                        ;; HERE is where we'd pull AS MANY components of `path` as WE need, and pass the rest off to
                        ;; the child router.
                        (let [my-path-element (first path)]
                          (js/console.log "Top router consumed path element(s):" my-path-element))
                        (when child-router
                          (route-entered child-router (rest path)))))]}
  (let [f (prim/get-state this :factory)]
    (when f (f child))))

(def ui-router (prim/factory TopRouter))

(defn notify-route-changed
  "Notify the child router of component that the route has changed to new-path."
  [component new-path]
  (let [reconciler        (prim/get-reconciler component)
        state-map         @(prim/app-state reconciler)
        query             (prim/get-query component state-map)
        next-router-class (find-next-router query)
        next-router       (prim/class->any reconciler next-router-class)]
    (route-entered next-router new-path)))

(defsc Root [this {:keys [router]}]
  {:query         [{:router (prim/get-query TopRouter)}]
   :initial-state {:router {}}}
  (dom/div
    (dom/h1 "Router Demo")
    (dom/button {:onClick (fn [] (js/console.log (find-next-router (prim/get-query Root))))} "Root..find router")
    (dom/button {:onClick (fn [] (js/console.log (find-next-router (prim/get-query TopRouter))))} "From top router, find router")
    (dom/button {:onClick
                 (fn []
                   (let [state (prim/app-state (prim/get-reconciler this))]
                     (swap! state (fn [s] (-> s
                                            (m/integrate-ident* [:X 1] :replace [:top-router 1 :x]))))
                     (prim/set-query! this TopRouter {:query [{:x (prim/get-query X)}]})
                     (notify-route-changed this ["account" "1" "user" 3])))}
      "Simulate DYNAMIC route change to default")
    (dom/button {:onClick
                 (fn []
                   (let [state (prim/app-state (prim/get-reconciler this))]
                     (swap! state (fn [s] (-> s
                                            (m/integrate-ident* [:other-router 1] :replace [:top-router 1 :x])
                                            (assoc-in [:other-router 1] {}))))
                     (prim/set-query! this TopRouter {:query [{:x (prim/get-query OtherRouter)}]})
                     (notify-route-changed this ["account" "1" "user" 3])))}
      "Simulate DYNAMIC route change to other")
    (ui-router router)))

(ws/defcard router-demo-card
  {::wsm/card-width  2
   ::wsm/align       {:flex 1}
   ::wsm/card-height 13}
  (ct.fulcro/fulcro-card
    {::f.portal/root       Root
     ::f.portal/wrap-root? false
     ::f.portal/app        {:started-callback (fn [{:keys [reconciler]}])
                            :networking       (server/new-server-emulator (server/fulcro-parser) 300)}}))

;; ================================================================================
;; Routing experiment 2 (see adoc file)
;; ================================================================================

(declare User Settings Root2 RootRouter2)

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

(defn route-target? [c] (implements? RouteTarget c))

;; NON-static protocol for interacting as a route target
(defprotocol RouteLifecycle
  (will-leave [this props] "If a route target implements this non-static protocol, then `will-leave` will be called with
  the current component and props. If it returns `true` then the routing operation will continue.  If it returns `false`
  then whatever new route was requested will be completely abandoned.  It is the responsibility of this method to give
  UI feedback as to why the route change was aborted."))

(defn route-lifecycle? [c] (implements? RouteLifecycle c))

(defprotocol Router
  (get-targets [_] "Returns a set of classes to which this router routes."))

(defn route-immediate [ident] (with-meta ident {:immediate true}))
(defn route-deferred [ident] (with-meta ident {:immediate false}))
(defn immediate? [ident] (some-> ident meta :immediate))

(defn- apply-route* [state-map {:keys [router target]}]
  (let [router-class (log/spy :info (-> router meta :component))
        target-class (log/spy :info (-> target meta :component))]
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
    (if (log/spy :info router-id)
      (apply-route* state-map (get-in state-map [::id router-id ::pending-route]))
      state-map)))

(defmutation target-ready [{:keys [target]}]
  (action [{:keys [state]}]
    (log/info "Target ready " target)
    (swap! state target-ready* target))
  (refresh [_] [::current-route]))

(defn router? [component] (and component (implements? Router component)))

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

(deftest matching-prefix-tests
  (assertions
    "Can match a parameterized path segment"
    (matching-prefix ["user" :user-id] ["user" 1]) => ["user" "1"]
    "Can match a parameterized path segment with extra path elements"
    (matching-prefix ["user" :user-id] ["user" 1 "blah"]) => ["user" "1"]
    "returns an nil if there is no match"
    (matching-prefix ["user" :user-id] ["settings"]) => nil))

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
    (let [targets (get-targets router-class)]
      (reduce (fn [result target-class]
                (let [prefix (and target-class (route-target? target-class)
                               (some-> target-class (route-segment) (matching-prefix path)))]
                  (if (seq prefix)
                    (reduced {:matching-prefix prefix
                              :target          target-class})
                    result))) nil targets))))

(defn accepts-route?
  "Returns true if the given component is a router that manages a route target that will accept the given path."
  [component path]
  (boolean (route-target component path)))

(deftest accepts-route-tests
  (assertions
    "correctly identifies when a router can consume the given path."
    (accepts-route? RootRouter2 ["booga"]) => false
    (accepts-route? RootRouter2 ["settings"]) => true
    (accepts-route? RootRouter2 ["user"]) => false
    (accepts-route? RootRouter2 ["user" "1"]) => true))

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
    (some ast-node-for-route children)))

(deftest ast-node-for-route-test
  (let [ast (prim/query->ast (prim/get-query Root2))]
    (assertions
      "returns nil for invalid routes"
      (ast-node-for-route ast ["booga"]) => nil
      "Can find the router for a given path"
      (:component (ast-node-for-route ast ["user" "1"])) => RootRouter2)))

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
                prefix-length  (count matching-prefix)
                remaining-path (vec (drop prefix-length path))
                segment        (route-segment (log/spy :info target))
                params         (reduce
                                 (fn [p [k v]] (if (keyword? k) (assoc p k v) p))
                                 {}
                                 (map (fn [a b] [a b]) segment matching-prefix))
                router-ident   (prim/get-ident component {})
                target-ident   (will-enter target reconciler (log/spy :info params))]
            (if (immediate? target-ident)
              (swap! route-tx into `[(apply-route ~{:router (with-meta router-ident {:component component})
                                                    :target (with-meta target-ident {:component target})}) ~router-ident])
              (swap! route-tx into `[(mark-route-pending ~{:router (with-meta router-ident {:component component})
                                                           :target (with-meta target-ident {:component target})})]))
            (when (seq remaining-path)
              (recur (ast-node-for-route root remaining-path) remaining-path)))))
      (prim/transact! reconciler (log/spy :info @route-tx)))))

(defsc Settings [this {:keys [:x] :as props}]
  {:query         [:x]
   :ident         (fn [] [:COMPONENT/by-id :settings])
   :initial-state {:x :param/x}
   :protocols     [static RouteTarget
                   (route-segment [_] ["settings"])
                   (will-enter [_ _ _] (route-immediate [:COMPONENT/by-id :settings]))
                   RouteLifecycle
                   (will-leave [this props] true)]}
  (dom/div (str "Settings: x = " x)))

(defsc User [this {:keys [user/id user/name] :as props}]
  {:query     [:user/id :user/name]
   :ident     [:user/id :user/id]
   :protocols [static RouteTarget
               (route-segment [_] ["user" :user-id])
               (will-enter [_ reconciler {:keys [user-id]}]
                 (let [id 1]
                   (df/load reconciler [:user/id id] User {:post-mutation        `target-ready
                                                           :post-mutation-params {:target [:user/id id]}})
                   (route-deferred [:user/id id])))
               RouteLifecycle
               (will-leave [this props] true)]
   }
  (dom/div (str "User: name = " name)))

(defsc RootRouter2 [this {::keys [id current-route] :as props}]
  {:query         [::id {::current-route (prim/get-query Settings)}]
   :ident         (fn [] [::id "RootRouter2"])              ; routers are singletons? Is that acceptable?
   :protocols     [static Router (get-targets [c] #{User Settings})]
   :initial-state (fn [params]
                    {::id            "RootRouter2"
                     ::current-route (prim/get-initial-state Settings params)})}
  (if-let [class (current-route-class this)]
    (let [factory (prim/factory class)]
      (factory current-route))
    (dom/div "Internal Router error. No current route.")))

(def ui-root-router-2 (prim/factory RootRouter2))

(defsc Root2 [this {:keys [router route] :as props}]
  {:query         [:route {:router (prim/get-query RootRouter2)}]
   :initial-state {:router {:x 2}
                   :route  ["settings"]}}
  (dom/div
    (dom/div "Current route: " (pr-str route))
    (dom/button {:onClick (fn []
                            (change-route this ["user" 1])
                            )} "Change route to /user/1")
    (dom/button {:onClick (fn []
                            (change-route this ["settings"])
                            )} "Change route to /settings")
    (ui-root-router-2 router)))

(server/defquery-entity :user/id
  (value [env id params]
    {:user/id   id
     :user/name (str "User " id)}))

(ws/defcard router-2-demo-card
  {::wsm/card-width  2
   ::wsm/align       {:flex 1}
   ::wsm/card-height 13}
  (ct.fulcro/fulcro-card
    {::f.portal/root       Root2
     ::f.portal/wrap-root? false
     ::f.portal/app        {:started-callback (fn [{:keys [reconciler]}])
                            :networking       (server/new-server-emulator (server/fulcro-parser) 2000)}}))
