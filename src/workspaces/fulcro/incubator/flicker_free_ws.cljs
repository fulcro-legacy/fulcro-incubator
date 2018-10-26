(ns fulcro.incubator.flicker-free-ws
  (:require
    [fulcro.client.data-fetch :as df]
    [fulcro.client.dom :as dom]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.incubator.io-progress :as ip]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.server :as server :refer [defquery-root]]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [nubank.workspaces.model :as wsm]))

(defquery-root :random-items
  (value [_ _]
    (mapv
      (fn [i] {:db/id i :item/label (str (rand-int 1000) " Random thing")})
      (range (inc (rand-int 30))))))

(server/defmutation pretend-to-mark [_]
  (action [_] (js/console.log "marking...")))

(defmutation pretend-to-mark [{:keys [id new-value]}]
  (ok-action [{:keys [state]}]
    (js/console.log "ok action")
    (swap! state assoc-in [:todo-item/by-id id :item/done?] new-value))
  (slow-remote [env] (pm/pessimistic-mutation env)))

(defsc TodoItem [this {:keys [item/label item/done?]}]
  {:query              [:db/id :item/label :item/done?
                        ::pm/mutation-response]
   :ident              [:todo-item/by-id :db/id]
   :componentDidUpdate (fn [pp _] (ip/update-loading-visible! this pp))
   :initial-state      {:db/id :param/id :item/label :param/label :item/done? false}}
  (let [delayed?  (prim/get-state this :loading-visible?)
        disabled? (ip/busy? this)]
    (dom/li
      (dom/input {:type     "checkbox"
                  :disabled disabled?
                  :checked  done?
                  :onChange (fn [] (pm/pmutate! this `pretend-to-mark {:new-value (not done?)}))})
      (when delayed?
        (dom/span "..."))
      label)))

(def ui-todo-item (prim/factory TodoItem {:keyfn :db/id}))

(defsc TodoList [this {:keys [list/title list/items]}]
  {:query              [
                        [df/marker-table '_]
                        ::pm/mutation-response
                        :db/id
                        :list/title
                        {:list/items (prim/get-query TodoItem)}]
   :ident              [:todo-list/by-id :db/id]
   :componentDidUpdate (fn [pp _] (ip/update-loading-visible! this pp))
   :initial-state      {:db/id 1 :list/title "My List" :list/items [{:id 1 :label "Stuff"}
                                                                    {:id 2 :label "Stuff"}]}}
  (let [show-loading?   (prim/get-state this :loading-visible?)
        io-in-progress? (ip/busy? this)
        my-ident        (prim/get-ident this)]
    (dom/div
      (dom/button {:disabled io-in-progress?
                   :onClick  #(df/load this :random-items TodoItem
                                {:marker my-ident
                                 :remote :fast-remote
                                 :target (conj my-ident :list/items)})} "Load List (from fast server)")
      (dom/button {:disabled io-in-progress?
                   :onClick  #(df/load this :random-items TodoItem
                                {:marker my-ident
                                 :remote :slow-remote
                                 :target (conj my-ident :list/items)})} "Load List (from slow server)")
      (dom/h2 title)
      (when show-loading?
        (dom/div "Loading..."))
      (dom/ul
        (map ui-todo-item items)))))

(ws/defcard flicker-free-progress-card
  {::wsm/card-width  4
   ::wsm/align       {:flex 1}
   ::wsm/card-height 2}
  (ct.fulcro/fulcro-card
    {::f.portal/root       TodoList
     ::f.portal/wrap-root? true
     ::f.portal/app        {:networking {:fast-remote (server/new-server-emulator (server/fulcro-parser) 50)
                                         :slow-remote (server/new-server-emulator (server/fulcro-parser) 2000)}}}))

