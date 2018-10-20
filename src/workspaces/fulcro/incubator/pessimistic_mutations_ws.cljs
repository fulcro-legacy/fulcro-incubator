(ns fulcro.incubator.pessimistic-mutations-ws
  (:require
    [nubank.workspaces.core :as ws]
    [fulcro.client.primitives :as fp :refer [defsc]]
    [fulcro.server :as server]
    [fulcro-spec.core :refer [specification assertions]]
    [fulcro.incubator.pessimistic-mutations :as i.pm]
    [fulcro.client.dom :as dom]
    [nubank.workspaces.model :as wsm]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]))

(server/defmutation do-something-good [_]
  (action [env]
    (js/console.log "server do something good")))

(server/defmutation do-something-bad [_]
  (action [env]
    (throw (ex-info "You screwed up!!!" {}))))

(server/defmutation do-something-sorta-bad [_]
  (action [env]
    {:com.wsscode.pathom.core/mutation-errors :error-33}))

(i.pm/defpmutation do-something-good [_]
  (remote [_] true))

(i.pm/defpmutation do-something-bad [_]
  (remote [_] true))

(i.pm/defpmutation do-something-sorta-bad [_]
  (remote [_] true))

(defsc DemoComponent [this props]
  {:query         [:demo/id :ui/checked?]
   :ident         [:demo/id :demo/id]
   :initial-state {:demo/id 1 :ui/checked? true}}
  (dom/div
    (dom/button {:onClick #(i.pm/pmutate! this `do-something-bad {})} "Mutation Crash/Hard network error")
    (dom/button {:onClick #(i.pm/pmutate! this `do-something-sorta-bad {})} "API Level Mutation Error")
    (dom/button {:onClick #(i.pm/pmutate! this `do-something-good {})} "Good Mutation")
    "Hi"))

(ws/defcard pmutation-card
  {::wsm/card-width 2 ::wsm/card-height 13}
  (ct.fulcro/fulcro-card
    {::f.portal/root       DemoComponent
     ::f.portal/wrap-root? true
     ::f.portal/app        {:networking (server/new-server-emulator (server/fulcro-parser) 300)}}))
