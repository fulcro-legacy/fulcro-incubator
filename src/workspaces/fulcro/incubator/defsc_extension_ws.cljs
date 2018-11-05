(ns fulcro.incubator.defsc-extension-ws
  (:require-macros
    [fulcro.incubator.defsc-foo :refer [defsc-foo]])
  (:require
    [fulcro.client.data-fetch :as df]
    [fulcro.client.dom :as dom]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.incubator.io-progress :as ip]
    [fulcro.incubator.other-protocol :refer [Other special]]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.server :as server :refer [defquery-root]]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [nubank.workspaces.model :as wsm]))

(defsc-foo TestComponent [this props]
  {:query         [:a],
   :ident         (fn [] [:table 1]),
   :initial-state {:a 1},
   :special       (fn [] (js/console.log "Did special thing!"))}
  (dom/div
    (dom/button {:onClick (fn [] (special TestComponent))} "Try it!!!")))

(ws/defcard extended-defsc-trial-card
  {::wsm/card-width  4
   ::wsm/align       {:flex 1}
   ::wsm/card-height 2}
  (ct.fulcro/fulcro-card
    {::f.portal/root       TestComponent
     ::f.portal/wrap-root? true}))
