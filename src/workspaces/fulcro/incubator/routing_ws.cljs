(ns fulcro.incubator.routing-ws
  (:require
    [fulcro.incubator.ui-state-machines :as uism :refer [defstatemachine]]
    [fulcro-spec.core :refer [assertions component]]
    [nubank.workspaces.core :as ws]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.server :as server]
    [fulcro-spec.core :refer [specification assertions]]
    [fulcro.client.dom :as dom]
    [nubank.workspaces.model :as wsm]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [fulcro.client.mutations :as m]
    [taoensso.timbre :as log]
    [fulcro.client.data-fetch :as df]
    [fulcro.incubator.dynamic-routing :as dr :refer [defrouter]]))

(declare change-route)

(defn ready-handler [env]
  (uism/apply-action env dr/target-ready* (uism/retrieve env :target)))

(defn fail-handler [env]
  (log/error "Route failed")
  env)

(defn route-handler [{::uism/keys [event-data] :as env}]
  (let [{:keys [router target]} event-data
        immediate? (dr/immediate? target)]
    (-> (if immediate?
          (-> env
            (uism/apply-action dr/apply-route* event-data)
            (uism/activate :routed))
          (-> env
            (uism/apply-action dr/mark-route-pending* event-data)
            (uism/set-timeout :error-timer :timeout! {} 500 #{:ready!})
            (uism/set-timeout :delay-timer :waiting! {} 50 #{:ready!})
            (uism/activate :deferred)))
      (uism/store :target target))))

(defstatemachine RouterStateMachine
  {::uism/actors
   #{:router}

   ::uism/aliases
   {:current-route [:router ::dr/current-route]
    :state         [:router ::dr/current-state]}

   ::uism/states
   {:initial  {::uism/handler route-handler}

    :deferred {::uism/events
               {:waiting! {::uism/target-state :pending}
                :ready!   {::uism/target-state :routed
                           ::uism/handler      ready-handler}
                :timeout! {::uism/target-state :failed
                           ::uism/handler      fail-handler}}}

    :pending  {::uism/events
               {:waiting! {::uism/target-state :pending}
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

;; TASK: Write a macro that simplifies this:
;; (defsc-router-target Pane2 [this props]
;;   {... normal defsc stuff...
;;    :route-segment ["pane2"]
;;    :will-enter (fn [c reconciler route-params] ...) ; optional, defaults to ident of component
;;    :will-leave (fn [this props] boolean) ; optional, defaults to true
;;   }
;;   ...normal body...)
(defsc Pane1 [this {:keys [:y] :as props}]
  {:query         [:y]
   :ident         (fn [] [:COMPONENT/by-id :pane1])
   :initial-state {:y 1}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["pane1"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :pane1]))
                   dr/RouteLifecycle
                   (will-leave [this props] true)]}
  (dom/div (str "PANE 1")))

(defsc Pane2 [this {:keys [:x] :as props}]
  {:query         [:x]
   :ident         (fn [] [:COMPONENT/by-id :pane2])
   :initial-state {:x 1}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["pane2"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :pane2]))
                   dr/RouteLifecycle
                   (will-leave [this props] true)]}
  (dom/div (str "PANE 2")))

(prim/defsc SettingsPaneRouter [this {::dr/keys [id current-route] :as props}]
  {:query         (fn [] [::dr/id [::uism/asm-id :SettingsPaneRouter] {::dr/current-route (prim/get-query Pane1)} {:alt0 (prim/get-query Pane2)}])
   :ident         (fn [] [::dr/id :SettingsPaneRouter])
   :protocols     [static dr/Router
                   (get-targets [_] #{Pane1 Pane2})]
   :initial-state (fn [params]
                    {::dr/id            :SettingsPaneRouter
                     ::dr/current-route (prim/get-initial-state Pane1 params)
                     :alt0              (prim/get-initial-state Pane1 {})})}
  (let [current-state (get-in props [[::uism/asm-id :SettingsPaneRouter] ::uism/active-state])]
    (log/info "Router " id "in state" current-state)
    (case current-state
      :initial (dom/div "Just Started.")
      :pending (dom/div "LOADING...")
      :failed (dom/div "Screwed.")
      :routed (if-let [class (dr/current-route-class this)]
                (let [factory (prim/factory class)]
                  (factory current-route)))
      nil)))

#_(defrouter SettingsPaneRouter Pane1 Pane2)

(def ui-settings-pane-router (prim/factory SettingsPaneRouter))

(defsc Settings [this {:keys [:x :panes] :as props}]
  {:query         [:x {:panes (prim/get-query SettingsPaneRouter)}]
   :ident         (fn [] [:COMPONENT/by-id :settings])
   :initial-state {:x     :param/x
                   :panes {}}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["settings"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :settings]))
                   dr/RouteLifecycle
                   (will-leave [this props] true)]}
  (dom/div
    (str "Settings: x = " x)
    (ui-settings-pane-router panes)))

(defsc User [this {:keys [user/id user/name] :as props}]
  {:query     [:user/id :user/name]
   :ident     [:user/id :user/id]
   :protocols [static dr/RouteTarget
               (route-segment [_] ["user" :user-id])
               (will-enter [_ reconciler {:keys [user-id]}]
                 (let [id 1]
                   (df/load reconciler [:user/id id] User {:post-mutation        `dr/target-ready
                                                           :post-mutation-params {:target [:user/id id]}})
                   (dr/route-deferred [:user/id id])))
               dr/RouteLifecycle
               (will-leave [this props] true)]}
  (dom/div (str "User: name = " name)))

; (defrouter RootRouter2 Settings User)
(prim/defsc RootRouter2 [this {::dr/keys [id current-route] :as props}]
  {:query         (fn [] [::dr/id [::uism/asm-id :RootRouter2] {::dr/current-route (prim/get-query Settings)} {:alt0 (prim/get-query User)}])
   :ident         (fn [] [::dr/id :RootRouter2])
   :protocols     [static dr/Router
                   (get-targets [_] #{Settings User})]
   :initial-state (fn [params]
                    {::dr/id            :RootRouter2
                     ::dr/current-route (prim/get-initial-state Settings params)
                     :alt0              (prim/get-initial-state User {})})}
  (let [current-state (get-in props [[::uism/asm-id :RootRouter2] ::uism/active-state])]
    (js/console.log props)
    (log/info "Router " id "in state" current-state)
    (case current-state
      :initial (dom/div "Just Started.")
      :pending (dom/div "LOADING...")
      :failed (dom/div "Screwed.")
      :routed (if-let [class (dr/current-route-class this)]
                (let [factory (prim/factory class)]
                  (factory current-route)))
      nil)))

(def ui-root-router-2 (prim/factory RootRouter2))

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
        root       (dr/ast-node-for-route ast new-route)]
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
        (when (and component (dr/router? component))
          (let [{:keys [target matching-prefix]} (dr/route-target component path)
                target-ast     (some-> target (prim/get-query state-map) prim/query->ast)
                prefix-length  (count matching-prefix)
                remaining-path (vec (drop prefix-length path))
                segment        (dr/route-segment+ target)
                params         (reduce
                                 (fn [p [k v]] (if (keyword? k) (assoc p k v) p))
                                 {}
                                 (map (fn [a b] [a b]) segment matching-prefix))
                router-ident   (prim/get-ident component {})
                router-id      (-> router-ident second)
                target-ident   (dr/will-enter+ target reconciler params)
                event-data     {:router (vary-meta router-ident assoc :component component)
                                :target (vary-meta target-ident assoc :component target)}]
            (if-not (uism/get-active-state reconciler router-id)
              (uism/begin! this RouterStateMachine router-id
                {:router (uism/with-actor-class router-ident component)}
                event-data)
              (uism/trigger! reconciler router-id :route! event-data))
            (when (seq remaining-path)
              (recur (dr/ast-node-for-route target-ast remaining-path) remaining-path))))))))

(defsc Root2 [this {:keys [router route] :as props}]
  {:query             [:route {:router (prim/get-query RootRouter2)}]
   :componentDidMount (fn [] (change-route this ["settings"]))
   :initial-state     {:router {:x 2}
                       :route  ["settings"]}}
  (dom/div
    (dom/div "Current route: " (pr-str route))
    (dom/button {:onClick (fn []
                            (change-route this ["user" 1])
                            )} "Change route to /user/1")
    (dom/button {:onClick (fn [] (change-route this ["settings"]))} "Change route to /settings")
    (dom/button {:onClick (fn [] (change-route this ["settings" "pane1"]))} "Change route to /settings/pane1")
    (dom/button {:onClick (fn [] (change-route this ["settings" "pane2"]))} "Change route to /settings/pane2")
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
                            :networking       (server/new-server-emulator (server/fulcro-parser) 1000)}}))
