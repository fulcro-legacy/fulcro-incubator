(ns fulcro.incubator.state-machine-ws
  (:require
    [fulcro.client.cards :refer [defcard-fulcro make-root]]
    [fulcro.client.dom :as dom]
    [fulcro.client.mutations :refer [defmutation]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.server :as server]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.incubator.mutation-interface :as mi]
    [nubank.workspaces.card-types.fulcro :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [nubank.workspaces.lib.fulcro-portal :as f.portal]
    [nubank.workspaces.model :as wsm]
    [fulcrologic.semantic-ui.factories :as sui]
    [taoensso.timbre :as log]
    [fulcro.incubator.ui-state-machines :as uism]
    [fulcro.events :as evt]))

;; ================================================================================
;; Sample REUSABLE Machine Definition
;; ================================================================================

(uism/defstatemachine login-machine
  {::uism/actor-names #{:dialog :form}
   ;; Every fulcro state value that is used by plugins MUST have an alias.
   ::uism/aliases     {:visible?       [:dialog :ui/active?]
                       :login-enabled? [:form :ui/login-enabled?]
                       :busy?          [:form :ui/busy?]
                       :error          [:form :ui/login-error]
                       :username       [:form :user/email]
                       :password       [:form :user/password]}
   ;; Plugs receive a map keyed by *alias* (e.g. :username) whose values are the real values from Fulcro state.
   ;; This allows the plugins to have reasonable defaults without knowing the details of the real data model.
   ::uism/plugins     {:valid-credentials? (fn [{:keys [username password]}]
                                             (and (seq username) (seq password)))}
   ::uism/events      #{:login! :success :failure}
   ::uism/states      {:initial
                       {::uism/handler (fn [env]
                                         (log/info "Initial state.")
                                         (-> env
                                           (uism/set-aliased-value :visible? true)
                                           (uism/set-aliased-value :login-enabled? false)
                                           (uism/set-aliased-value :username "")
                                           (uism/set-aliased-value :password "")
                                           (uism/activate :filling-info)))}

                       :filling-info
                       {::uism/handler
                        (fn [{::uism/keys [event-id] :as env}]
                          (log/info "Filling info: " event-id)
                          (let [valid?   (uism/run env :valid-credentials?)
                                enabled? (uism/alias-value env :login-enabled?)]
                            (cond-> env
                              (not= valid? enabled?)
                              (uism/set-aliased-value :login-enabled? valid?)

                              (and valid? (= event-id :login!))
                              (->
                                (uism/set-aliased-value :login-enabled? false)
                                (uism/set-aliased-value :error "")
                                (uism/set-aliased-value :busy? true)
                                (uism/activate :attempting-login)))))}

                       :attempting-login
                       {::uism/handler (fn [{::uism/keys [event-id event-data] :as env}]
                                         (log/info "Attempting login: " event-id)
                                         (case event-id
                                           :success (-> env
                                                      (uism/set-aliased-value :busy? false)
                                                      (uism/set-aliased-value :visible? false)
                                                      (uism/exit))
                                           :failure (-> env
                                                      (uism/set-aliased-value :error "Invalid credentials. Please try again.")
                                                      (uism/set-aliased-value :busy? false)
                                                      (uism/activate :filling-info))
                                           env))}}})

;; You COULD make a new (derived) state machine def by deep-merging against the above..mainly to
;; override the meaning of aliases. Not necessary in this case.
(defmutation login [_]
  (action [{:keys [reconciler]}]
    (uism/trigger! reconciler ::loginsm :login!))
  (ok-action [{:keys [reconciler]}]
    (uism/trigger! reconciler ::loginsm :success))
  (error-action [{:keys [reconciler]}]
    (uism/trigger! reconciler ::loginsm :failure))
  (remote [env] (pm/pessimistic-mutation env)))
(mi/declare-mutation login `login)

(defn login! [this {:keys [user/email user/password]}]
  (let []
    (pm/pmutate! this `login {:username email :password password})))

(defsc LoginForm [this {:keys [ui/login-enabled? ui/login-error ui/busy? user/email user/password] :as props}]
  {:query         [:ui/login-enabled? :ui/login-error :ui/busy? :user/email :user/password]
   :ident         (fn [] [:COMPONENT/by-id ::login])
   :initial-state {:user/email "" :user/password ""}}
  (let [error?        (seq login-error)
        error-classes [(when error? "error")]]
    (dom/div :.ui.container.form {:classes (into error-classes [(when busy? "loading")])}
      (dom/div :.field {:classes error-classes}
        (dom/label "Email")
        (dom/input {:value     email
                    :onKeyDown (fn [evt] (when (evt/enter? evt) (login! this props)))
                    :onChange  (fn [evt] (uism/set-string! this ::loginsm :username evt))}))
      (dom/div :.field {:classes error-classes}
        (dom/label "Password")
        (dom/input {:value     password
                    :onKeyDown (fn [evt]
                                 (when (evt/enter? evt) (login! this props)))
                    :onChange  (fn [evt] (uism/set-string! this ::loginsm :password evt))}))
      (dom/div :.field
        (dom/button {:disabled (not login-enabled?)
                     :onClick  (fn [] (login! this props))}
          "Login"))
      (when error?
        (dom/div :.ui.error.message
          (dom/p login-error))))))

(def ui-login-form (prim/factory LoginForm {:keyfn :db/id}))

(defsc Dialog [_ {:keys [ui/active? dialog/form]}]
  {:query         [:ui/active? {:dialog/form (prim/get-query LoginForm)}]
   :ident         (fn [] [:COMPONENT/by-id ::dialog])
   :initial-state {:ui/active? false :dialog/form {}}}
  (sui/ui-modal {:open active?}
    (sui/ui-modal-header {} "Login")
    (sui/ui-modal-content {}
      (ui-login-form form))))

(def ui-dialog (prim/factory Dialog {:keyfn :ui/active?}))

(defsc Root [this {:keys [root/dialog]}]
  {:query         [{:root/dialog (prim/get-query Dialog)}]
   :initial-state {:root/dialog {}}}
  (dom/div nil
    (dom/button
      {:onClick (fn []
                  (uism/begin! this login-machine ::loginsm {:dialog (prim/get-ident Dialog {})
                                                             :form   (prim/get-ident LoginForm {})}))}
      "Start state machine")
    (ui-dialog dialog)))

(server/defmutation login [{:keys [username password]}]
  (action [_]
    (if (= username "tony")
      {}
      {::pm/mutation-errors "No way man!"})))

(ws/defcard state-machine-demo-card
  {::wsm/card-width  4
   ::wsm/align       {:flex 1}
   ::wsm/card-height 4}
  (ct.fulcro/fulcro-card
    {::f.portal/root       Root
     ::f.portal/wrap-root? false
     ::f.portal/app        {:networking (server/new-server-emulator (server/fulcro-parser) 1000)}}))
