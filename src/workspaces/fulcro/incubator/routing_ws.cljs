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
                   (will-leave [this props]
                     (js/console.log "Leaving pane1")
                     true)]}
  (dom/div (str "PANE 1")))

(declare SettingsPaneRouter)

(defsc Pane2 [this {:keys [:x] :as props}]
  {:query         [:x]
   :ident         (fn [] [:COMPONENT/by-id :pane2])
   :initial-state {:x 1}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["pane2"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :pane2]))
                   dr/RouteLifecycle
                   (will-leave [this props] (js/console.log "Deny pane2")
                     true)]}
  (dom/div
    (dom/button {:onClick #(dr/change-route-relative this SettingsPaneRouter ["pane1"])} "Relative route to pane 1")
    (str "PANE 2")))

(defrouter SettingsPaneRouter [this props]
  {:router-targets [Pane1 Pane2]})

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
                   (will-leave [this props] (js/console.log "Leaving settings") true)]}
  (dom/div
    (str "Settings: x = " x)
    (ui-settings-pane-router panes)))

(defsc User [this {:keys [user/id user/name] :as props}]
  {:query     [:user/id :user/name]
   :ident     [:user/id :user/id]
   :protocols [static dr/RouteTarget
               (route-segment [_] ["user" :user-id])
               (will-enter [_ reconciler {:keys [user-id]}]
                 (when-let [user-id (some-> user-id (js/parseInt))]
                   (dr/route-deferred [:user/id user-id]
                     #(df/load reconciler [:user/id user-id] User {:post-mutation        `dr/target-ready
                                                                   :marker               false
                                                                   :post-mutation-params {:target [:user/id user-id]}}))))
               dr/RouteLifecycle
               (will-leave [this props] (js/console.log "Leaving user " (:user/id props)) true)]}
  (dom/div (str "User: name = " name)))

(defrouter RootRouter2 [this {:keys [current-state pending-path-segment]}]
  {:router-targets     [Settings User]
   :componentDidUpdate (fn [pp ps]
                         (let [current-state        (uism/get-active-state this :RootRouter2)
                               sm-env               (uism/state-machine-env (prim/component->state-map this)
                                                      nil :RootRouter2 :noop {})
                               pending-path-segment (uism/retrieve sm-env :pending-path-segment)
                               current-path         (uism/retrieve sm-env :path-segment)]
                           (js/console.log :rr2-updated current-state :pending-path pending-path-segment
                             :current-path current-path)))}
  (case current-state
    :pending (dom/div "Loading a user..."
               (dom/button {:onClick #(dr/change-route this ["settings" "pane2"])} "cancel"))
    :failed (do
              (dom/div "Ooops!")
              (dom/button {:onClick #(dr/change-route this ["settings"])} "Go to settings"))
    (dom/div "...")))

(def ui-root-router-2 (prim/factory RootRouter2))

(defsc Root2 [this {:keys [router] :as props}]
  {:query         [{:router (prim/get-query RootRouter2)}]
   :initial-state {:router {:x 2}}}
  (dom/div
    (dom/button {:onClick (fn [] (dr/change-route this ["user" 1]))} "Change route to /user/1")
    (dom/button {:onClick (fn [] (dr/change-route this ["settings"]))} "Change route to /settings")
    (dom/button {:onClick (fn [] (dr/change-route this ["settings" "pane1"]))} "Change route to /settings/pane1")
    (dom/button {:onClick (fn [] (dr/change-route this ["settings" "pane2"]))} "Change route to /settings/pane2")
    (dom/button {:onClick (fn [] (js/console.log (dr/current-route this this)))} "Log current route")
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
     ::f.portal/app        {:started-callback (fn [{:keys [reconciler]}]
                                                (dr/change-route reconciler ["settings" "pane1"]))
                            :networking       (server/new-server-emulator (server/fulcro-parser) 1000)}}))
