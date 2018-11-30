(ns fulcro.incubator.dynamic-routing-test
  (:require
    [clojure.test]
    [nubank.workspaces.core :refer [deftest]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.data-fetch :as df]
    [fulcro-spec.core :refer [assertions]]
    [fulcro.incubator.dynamic-routing :as dr]))

(declare User Settings Root2 RootRouter2 SettingsPaneRouter Pane1 Pane2)

(deftest route-target-detection-test
  (assertions
    (dr/route-target? RootRouter2) => false
    (dr/route-target? Root2) => false
    (dr/route-target? Settings) => true))

(deftest route-lifecycle-detection-test
  (assertions
    (dr/route-lifecycle? RootRouter2) => false
    (dr/route-lifecycle? User) => true))

(deftest router-detection-test
  (assertions
    (dr/router? RootRouter2) => true
    (dr/router? User) => false))

(deftest matching-prefix-tests
  (assertions
    "Can match a parameterized path segment"
    (dr/matching-prefix ["user" :user-id] ["user" 1]) => ["user" "1"]
    "Can match a parameterized path segment with extra path elements"
    (dr/matching-prefix ["user" :user-id] ["user" 1 "blah"]) => ["user" "1"]
    "returns an nil if there is no match"
    (dr/matching-prefix ["user" :user-id] ["settings"]) => nil))

(deftest accepts-route-tests
  (assertions
    "correctly identifies when a router can consume the given path."
    (dr/accepts-route? RootRouter2 ["booga"]) => false
    (dr/accepts-route? RootRouter2 ["settings"]) => true
    (dr/accepts-route? RootRouter2 ["user"]) => false
    (dr/accepts-route? RootRouter2 ["user" "1"]) => true))

(deftest ast-node-for-route-test
  (let [ast          (prim/query->ast (prim/get-query Root2))
        settings-ast (prim/query->ast (prim/get-query Settings))]
    (assertions
      "returns nil for invalid routes"
      (dr/ast-node-for-route ast ["booga"]) => nil
      "Can find the router for a given path"
      (:component (dr/ast-node-for-route ast ["user" "1"])) => RootRouter2

      (:component (dr/ast-node-for-route settings-ast ["pane1"])) => SettingsPaneRouter
      (:component (dr/ast-node-for-route settings-ast ["pane2"])) => SettingsPaneRouter)))

(defsc Pane1 [this {:keys [:y] :as props}]
  {:query         [:y]
   :ident         (fn [] [:COMPONENT/by-id :pane1])
   :initial-state {:y 1}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["pane1"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :pane1]))
                   static dr/RouteLifecycle
                   (will-leave [this props] true)]})

(defsc Pane2 [this {:keys [:x] :as props}]
  {:query         [:x]
   :ident         (fn [] [:COMPONENT/by-id :pane2])
   :initial-state {:x 1}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["pane2"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :pane2]))
                   static dr/RouteLifecycle
                   (will-leave [this props] true)]})

(defsc SettingsPaneRouter [this {::keys [id current-route] :as props}]
  ;; TASK: fix initial state hack
  {:query         [::id {::current-route (prim/get-query Pane1)} {::b (prim/get-query Pane2)}]
   :ident         (fn [] [::id "SettingsPaneRouter"])
   :protocols     [static dr/Router (get-targets [c] #{Pane1 Pane2})]
   :initial-state (fn [params]
                    {::id            "SettingsPaneRouter"
                     ::b             (prim/get-initial-state Pane2 {})
                     ::current-route (prim/get-initial-state Pane1 params)})})

(defsc Settings [this {:keys [:x :panes] :as props}]
  {:query         [:x {:panes (prim/get-query SettingsPaneRouter)}]
   :ident         (fn [] [:COMPONENT/by-id :settings])
   :initial-state {:x     :param/x
                   :panes {}}
   :protocols     [static dr/RouteTarget
                   (route-segment [_] ["settings"])
                   (will-enter [_ _ _] (dr/route-immediate [:COMPONENT/by-id :settings]))
                   static dr/RouteLifecycle
                   (will-leave [this props] true)]})

(defsc User [this {:keys [user/id user/name] :as props}]
  {:query     [:user/id :user/name]
   :ident     [:user/id :user/id]
   :protocols [static dr/RouteTarget
               (route-segment [_] ["user" :user-id])
               (will-enter [_ reconciler {:keys [user-id]}]
                 (let [id 1]
                   (df/load reconciler [:user/id id] User {:post-mutation        `target-ready
                                                           :post-mutation-params {:target [:user/id id]}})
                   (dr/route-deferred [:user/id id])))
               static dr/RouteLifecycle
               (will-leave [this props] true)]})

(defsc RootRouter2 [this {::keys [id current-route] :as props}]
  {:query         [::id {::current-route (prim/get-query Settings)}]
   :ident         (fn [] [::id "RootRouter2"])              ; routers are singletons? Is that acceptable?
   :protocols     [static dr/Router (get-targets [c] #{User Settings})]
   :initial-state (fn [params]
                    {::id            "RootRouter2"
                     ::current-route (prim/get-initial-state Settings params)})})

(defsc Root2 [this {:keys [router route] :as props}]
  {:query         [:route {:router (prim/get-query RootRouter2)}]
   :initial-state {:router {:x 2}
                   :route  ["settings"]}})


