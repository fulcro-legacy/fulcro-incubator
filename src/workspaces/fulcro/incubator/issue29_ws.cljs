(ns fulcro.incubator.issue29-ws
  (:require [clojure.core.async :refer [<! timeout chan]]
            [fulcro.client :as fc]
            [fulcro.client.primitives :as prim :refer [defsc]]
            [fulcro.client.dom :as dom]
            [fulcro.client.mutations :refer [defmutation]]
            [fulcro.incubator.pessimistic-mutations :as pm]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [com.wsscode.pathom.fulcro.network :as pn]
            [fulcro.incubator.ui-state-machines :as uism :refer [defstatemachine]]
            [nubank.workspaces.core :as ws]
            [nubank.workspaces.model :as wsm]
            [nubank.workspaces.lib.fulcro-portal :as f.portal]
            [nubank.workspaces.card-types.fulcro :as ct.fulcro])
  (:require-macros [clojure.core.async :as a :refer [go]]))

(defonce app (atom nil))

(defstatemachine government
  {::uism/actor-names #{:capital :singletonia}
   ::uism/states
                      {:initial {::uism/events
                                 {::uism/started {::uism/target-state :default}}}
                       :default {::uism/events
                                 {:do-stuff {::uism/handler
                                             (fn [env]
                                               (let [capital-class (uism/actor-class env :capital)
                                                     temp-id       (prim/tempid)]
                                                 (-> env
                                                   (assoc-in [::uism/state-map :city/by-id temp-id] {:city/id temp-id})
                                                   (assoc-in [::uism/state-map :country/by-id :singletonia :country/capital] [:city/by-id temp-id])
                                                   (uism/trigger-remote-mutation
                                                     :singletonia
                                                     `establish
                                                     {:city/temp-id          temp-id
                                                      ::uism/mutation-remote :remote
                                                      ::pm/returning         capital-class
                                                      ::pm/target            [:country/by-id :singletonia :country/capital]}))))}}}}})

(defn identify-district
  [{:district/keys [type id]}]
  (if (#{:shopping-district} type)
    [type id]
    [:other-district id]))

(defsc ShoppingDistrict
  [this props]
  {:ident (fn [] (identify-district props))
   :query [:district/id
           :district/name
           :district/type]})

(defsc OtherDistrict
  [this props]
  {:ident (fn [] (identify-district props))
   :query [:district/id
           :district/name
           :district/type]})

(defsc District
  [this props]
  {:ident (fn [] (identify-district props))
   :query (fn []
            {:shopping-district (prim/get-query ShoppingDistrict)
             :other-district    (prim/get-query OtherDistrict)})})

(defsc City
  [this props]
  {:ident         [:city/by-id :city/id]
   :query         [:city/id
                   :city/name
                   {:city/districts (prim/get-query District)}]
   :initial-state (fn [p]
                    {:city/id :none})})

(defsc Singletonia
  [this props]
  {:ident         [:country/by-id :country/id]
   :query         [:country/id
                   :country/name
                   {:country/capital (prim/get-query City)}]
   :initial-state (fn [p] {:country/id      :singletonia
                           :country/name    "Singletonia"
                           :country/capital (prim/get-initial-state City {})})}
  (dom/div
    "lalala"))

(def ui-singletonia (prim/factory Singletonia))

(defsc Root
  [this {:keys [root/singletonia]}]
  {:query         [{:root/singletonia (prim/get-query Singletonia)}]
   :initial-state (fn [p]
                    {:root/singletonia (prim/get-initial-state Singletonia {})})}
  (dom/div
    (dom/button {:onClick #(uism/trigger! this :singletonian-gov :do-stuff)} "GO")
    (ui-singletonia singletonia)))

(defn fulcro-init
  [{:keys [reconciler]}]
  (uism/begin! reconciler government :singletonian-gov
    {:singletonia (uism/with-actor-class [:country/by-id :singletonia] Singletonia)
     :capital     (uism/with-actor-class [:city/by-id :none] City)}))

(pc/defmutation establish
  [{:keys [config]}
   {:keys [city/temp-id]}]
  {::pc/sym    `establish
   ::pc/params [:city/temp-id]}
  (go
    (<! (timeout 100))
    {:city/id        :patagonia
     :city/name      "Patagonia"
     :city/districts [{:district/id   9
                       :district/type :shopping-district
                       :district/name "District 9"}]
     ::prim/tempids  {temp-id :patagonia}}))

(def registry
  [establish])

(defn mock-parser
  [app extra-env]
  (p/parallel-parser
    {::p/env     (merge extra-env
                   {:app-atom                  app
                    ::p/reader                 [p/map-reader
                                                pc/all-parallel-readers]
                    ::pc/resolver-dispatch     pc/resolver-dispatch-embedded
                    ::pc/mutate-dispatch       pc/mutation-dispatch-embedded
                    ::pc/mutation-join-globals [::prim/tempids]
                    ::p/union-path             pn/fulcro-union-path})
     ::p/mutate  pc/mutate-async
     ::p/plugins [(pc/connect-plugin {::pc/register registry})
                  p/request-cache-plugin
                  (p/post-process-parser-plugin p/elide-not-found)]}))

(defn new-mock-remote
  [app]
  (pn/pathom-remote
    (mock-parser app {})))

(ws/defcard demo-mutation-response-key-card
  {::wsm/card-width 4 ::wsm/card-height 2}
  (ct.fulcro/fulcro-card
    {::f.portal/root       Root
     ::f.portal/wrap-root? false
     ::f.portal/app        {:started-callback fulcro-init
                            :networking       {:remote (new-mock-remote app)}}}))
