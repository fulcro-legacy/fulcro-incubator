(ns fulcro.incubator.pessimistic-mutations-ws
  (:require
    [nubank.workspaces.core :refer [deftest]]
    [fulcro-spec.core :refer [specification behavior component assertions when-mocking]]
    [cljs.test :refer [is testing]]
    [nubank.workspaces.core :as ws]
    [fulcro.client.primitives :as fp :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.server :as server :refer [defquery-root]]
    [fulcro-spec.core :refer [specification assertions]]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.incubator.io-progress :as ip]
    [fulcro.client.dom :as dom]
    [nubank.workspaces.model :as wsm]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [fulcro.client.mutations :as m]
    [fulcro.client.data-fetch :as df]
    [fulcro.client.primitives :as prim]
    [fulcro.incubator.mutation-interface :as mi]
    [fulcro.logging :as log]
    [clojure.string :as str]))

(defsc TodoItem [this {:keys [item/label item/done?]}]
  {:query [:db/id :item/label :item/done?
           ::pm/mutation-response]
   :ident [:todo-item/by-id :db/id]})

(defsc TodoList [this {:keys [list/title list/items] :as props}]
  {:query [:db/id :list/title {:list/items (prim/get-query TodoItem)}]
   :ident [:todo-list/by-id :db/id]})

(server/defmutation do-something-good [_]
  (action [env]
    (js/console.log "server do something good")
    {:db/id      1
     :list/name  "Honey Do"
     :list/items [{:db/id 2 :item/name "Buy Milk"}]}))

(server/defmutation do-something-bad [_]
  (action [env]
    (throw (ex-info "You screwed up!!!" {}))))

(server/defmutation do-something-sorta-bad [_]
  (action [env]
    {::pm/mutation-errors :error-33}))

(defmutation do-something-good [_]
  (action [env]
    (js/console.log "Optimistic"))
  (ok-action [{:keys [state ref]}]
    (js/console.log "OK Done: visible mutation response: " (get-in @state ref)))
  (error-action [env]
    (js/console.log "Ran due to error"))
  (remote [env]
    (pm/pessimistic-mutation env)))

(mi/declare-mutation do-something-good-interface `do-something-good)

(defmutation do-something-bad [_]
  (action [env]
    (js/console.log "Optimistic"))
  (ok-action [env]
    (js/console.log "OK Done"))
  (error-action [{:keys [state ref]}]
    (throw (ex-info "BOOM" {}))
    (js/console.log "visible mutation response: " (get-in @state ref))
    (js/console.log "Ran due to error"))
  (remote [env] (pm/pessimistic-mutation env)))

(defmutation do-something-sorta-bad [_]
  (action [env]
    (js/console.log "Optimistic"))
  (ok-action [env]
    (js/console.log "OK Done"))
  (error-action [{:keys [state ref]}]
    (js/console.log "visible mutation response: " (get-in @state ref))
    (js/console.log "Ran due to error"))
  (remote [env] (pm/pessimistic-mutation env)))

(defsc DemoComponent [this props]
  {:query         [:demo/id :ui/checked?]
   :ident         [:demo/id :demo/id]
   :initial-state {:demo/id 1 :ui/checked? true}}
  (dom/div
    (dom/button {:onClick #(pm/ptransact! this `[(do-something-good {})
                                                 (do-something-bad {::pm/key :Sad-face})
                                                 (do-something-good {})])} "Combo under ptransact!")
    (dom/button {:onClick #(pm/pmutate! this `do-something-bad {::pm/key :Sad-face})} "Mutation Crash/Hard network error")
    (dom/button {:onClick #(pm/pmutate! this `do-something-sorta-bad {::pm/key :Bummer})} "API Level Mutation Error")
    (dom/button {:onClick #(pm/pmutate! this do-something-good-interface {::pm/returning TodoList
                                                                          ::pm/key       :todo-list-key
                                                                          ::pm/target    (df/multiple-targets
                                                                                           [:main-list]
                                                                                           (df/append-to [:all-lists]))})} "Good Mutation")
    (dom/h1 "See Javascript Console for Behavior Output")))

(ws/defcard pmutation-card
  {::wsm/card-width 4 ::wsm/card-height 2}
  (ct.fulcro/fulcro-card
    {::f.portal/root       DemoComponent
     ::f.portal/wrap-root? true
     ::f.portal/app        {:started-callback (fn [{:keys [reconciler]}]
                                                (swap! (prim/app-state reconciler) assoc :all-lists []))
                            :networking       (server/new-server-emulator (server/fulcro-parser) 300)}}))

(defmutation broken-pmutation [_]
  (ok-action [env] (+ 1 1)))

(defmutation broken-pmutation-2 [_]
  (error-action [env] (+ 1 1)))

(defmutation ok-mutation [_]
  (remote [env] (pm/pessimistic-mutation env)))

(defmutation ok-mutation-2 [_]
  (ok-action [env] (inc 1))
  (remote [env] (pm/pessimistic-mutation env)))

(mi/declare-mutation a `ok-mutation)

(deftest pmutation?-test
  (behavior "Detects missing remote AST marker"
    (when-mocking
      (log/-log loc msg) => (assertions
                              "Logs an error about the missing remote AST markings"
                              (vector? loc) => true)

      (assertions
        "Returns false if the mutation does not modify the remote"
        (pm/pmutation? #{:remote} `broken-pmutation {}) => false))
    (when-mocking
      (log/-log loc msg) => (assertions
                              "Logs an error about the missing remote AST markings"
                              (vector? loc) => true)

      (assertions
        "Returns false if the mutation does not modify the remote"
        (pm/pmutation? #{:remote} `broken-pmutation-2 {}) => false)))
  (assertions
    "Detects mutations that have the correct remote marker"
    (pm/pmutation? #{:remote} `ok-mutation {}) => true
    (pm/pmutation? #{:remote} `ok-mutation-2 {}) => true
    "Accepts declared mutations"
    (pm/pmutation? #{:remote} a {}) => true))

(defmutation local-mutation [_]
  (action [_] (inc 1)))

(defmutation normal-remote [_]
  (action [_] (inc 1))
  (remote [_] true))

(def ui-todo (prim/factory TodoItem))

(deftest mixed-tx->ptransaction-test
  (when-mocking
    (prim/get-reconciler c) => {:config {:remotes #{:remote}}}
    (pm/get-ident c) => [:the :ident]

    (let [tx (pm/mixed-tx->ptransaction :fake-component `[(local-mutation {}) (ok-mutation {}) (normal-remote {})])]
      (assertions
        tx => '[(fulcro.incubator.pessimistic-mutations-ws/local-mutation {})
                (fulcro.incubator.pessimistic-mutations/start-pmutation {})
                (fulcro.incubator.pessimistic-mutations-ws/ok-mutation {})
                (fulcro.client.data-fetch/fallback
                  {:action                                     fulcro.incubator.pessimistic-mutations/mutation-network-error
                   :params                                     {}
                   :fulcro.incubator.pessimistic-mutations/ref [:the :ident]})
                (fulcro.incubator.pessimistic-mutations/finish-pmutation
                  {:mutation fulcro.incubator.pessimistic-mutations-ws/ok-mutation
                   :params   {}})
                (fulcro.incubator.pessimistic-mutations-ws/normal-remote {})]))))
