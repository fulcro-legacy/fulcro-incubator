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

(defstatemachine RouterStateMachine
  {::uism/actors
   #{:router}

   ::uism/aliases
   {:current-route [:router ::current-route]}

   ::uism/states
   {:initial
    {::uism/handler
     (fn [{::uism/keys [event-data] :as env}]
       (let [{:keys [targets]} event-data]
         (-> env
           (uism/store :targets (with-meta {} {:targets targets})))))}

    :pending
    {::uism/events
     {}}

    :failed
    {}

    :routed
    {}

    }})

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

(prim/defsc SettingsPaneRouter [this {::dr/keys [current-route]}]
  {:query         [::dr/id {::dr/current-route (prim/get-query Pane1)} {:alt0 (prim/get-query Pane2)}]
   :ident         (fn [] [::dr/id "SettingsPaneRouter"])
   :protocols     [static dr/Router
                   (get-targets [_] #{Pane1 Pane2})]
   :initial-state (fn [params]
                    {::dr/id            "SettingsPaneRouter"
                     ::dr/current-route (prim/get-initial-state Pane1 params)
                     :alt0              (prim/get-initial-state Pane1 {})})}
  (if-let [class (dr/current-route-class this)]
    (let [factory (prim/factory class)]
      (factory current-route))))

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
(prim/defsc RootRouter2 [this {::dr/keys [current-route]}]
  {:query         [::dr/id {::dr/current-route (prim/get-query Settings)} {:alt0 (prim/get-query User)}]
   :ident         (fn [] [::dr/id "RootRouter2"])
   :protocols     [static dr/Router
                   (get-targets [_] #{Settings User})]
   :initial-state (fn [params]
                    {::dr/id            "RootRouter2"
                     ::dr/current-route (prim/get-initial-state Settings params)
                     :alt0              (prim/get-initial-state User {})})}
  (if-let [class (dr/current-route-class this)]
    (let [factory (prim/factory class)]
      (factory current-route))))


(def ui-root-router-2 (prim/factory RootRouter2))

(defsc Root2 [this {:keys [router route] :as props}]
  {:query         [:route {:router (prim/get-query RootRouter2)}]
   :initial-state {:router {:x 2}
                   :route  ["settings"]}}
  (dom/div
    (dom/div "Current route: " (pr-str route))
    (dom/button {:onClick (fn []
                            (dr/change-route this ["user" 1])
                            )} "Change route to /user/1")
    (dom/button {:onClick (fn [] (dr/change-route this ["settings"]))} "Change route to /settings")
    (dom/button {:onClick (fn [] (dr/change-route this ["settings" "pane1"]))} "Change route to /settings/pane1")
    (dom/button {:onClick (fn [] (dr/change-route this ["settings" "pane2"]))} "Change route to /settings/pane2")
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
