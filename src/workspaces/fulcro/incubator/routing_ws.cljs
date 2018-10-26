(ns fulcro.incubator.routing-ws
  (:require
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
  {:query         ['*]
   :ident         (fn [] [:distant-router 1])
   :componentWillUnmount (fn [] (js/console.log "Distant router leaving the screen"))
   :initial-state {}
   :protocols     [static
                   Routing
                   (route-entered [this path]
                     (js/console.log "Distant router Entered with " path))]}
  (dom/div "DISTANT ROUTER CONTENT"))

(def ui-distant-router (prim/factory DistantRouter))

;; SEE TopRouter...this is just a placeholder, and doesn't show everything is should do
(defsc OtherRouter [this props]
  {:query         ['*]
   :ident         (fn [] [:other-router 1])
   :componentWillUnmount (fn [] (js/console.log "Other router leaving the screen"))
   :initial-state {}
   :protocols     [static
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
