(ns fulcro.incubator.pessimistic-mutations-ws
  (:require
    [nubank.workspaces.core :as ws]
    [fulcro.client.primitives :as fp :refer [defsc]]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.server :as server]
    [fulcro-spec.core :refer [specification assertions]]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.client.dom :as dom]
    [nubank.workspaces.model :as wsm]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [fulcro.client.mutations :as m]
    [fulcro.client.data-fetch :as df]
    [fulcro.client.primitives :as prim]
    [fulcro.incubator.mutation-interface :as mi]))

(defsc TodoItem [_ _]
  {:ident [:item/id :item/id]
   :query [:item/id :item/name]})

(defsc TodoList [_ _]
  {:ident [:list/id :list/id]
   :query [:list/id :list/name {:list/items (fp/get-query TodoItem)}]})

(server/defmutation do-something-good [_]
  (action [env]
    (js/console.log "server do something good")
    {:list/id    1
     :list/name  "Honey Do"
     :list/items [{:item/id 2 :item/name "Buy Milk"}]}))

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
    (dom/button {:onClick #(pm/pmutate! this `do-something-bad {::pm/key :Sad-face})} "Mutation Crash/Hard network error")
    (dom/button {:onClick #(pm/pmutate! this `do-something-sorta-bad {::pm/key :Bummer})} "API Level Mutation Error")
    (dom/button {:onClick #(pm/pmutate! this do-something-good-interface {::pm/returning TodoList
                                                                          ::pm/key       :todo-list-key
                                                                          ::pm/target    (df/multiple-targets
                                                                                           [:main-list]
                                                                                           (df/append-to [:all-lists]))})} "Good Mutation")
    "Hi"))

(ws/defcard pmutation-card
  {::wsm/card-width 2 ::wsm/card-height 13}
  (ct.fulcro/fulcro-card
    {::f.portal/root       DemoComponent
     ::f.portal/wrap-root? true
     ::f.portal/app        {:started-callback (fn [{:keys [reconciler]}]
                                                (swap! (prim/app-state reconciler) assoc :all-lists []))
                            :networking       (server/new-server-emulator (server/fulcro-parser) 300)}}))
