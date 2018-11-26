(ns fulcro.incubator.state-machine-ws
  (:require
    [fulcro.client :as fc]
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
    [fulcro.logging :as log]
    [fulcro.incubator.ui-state-machines :as uism]
    [fulcro.events :as evt]))

;; ================================================================================
;; Sample REUSABLE Machine Definition
;; ================================================================================

;; holder of app so aborts are possible on loads and mutations
(defonce my-app (atom nil))

(defsc Session [_ _]
  {:query [:username :logged-in?]
   :ident (fn [] [:globals ::session])})

(uism/defstatemachine login-machine
  {::uism/actor-names
   #{:dialog :form :session}

   ;; Every fulcro state value that is used by plugins MUST have an alias.
   ::uism/aliases
   {:visible?       [:dialog :ui/active?]
    :login-enabled? [:form :ui/login-enabled?]
    :logged-in?     [:session :logged-in?]
    :busy?          [:form :ui/busy?]
    :error          [:form :ui/login-error]
    :username       [:form :user/email]
    :password       [:form :user/password]}

   ;; Plugs receive a map keyed by *alias* (e.g. :username) whose values are the real values from Fulcro state.
   ;; This allows the plugins to have reasonable defaults without knowing the details of the real data model.
   ::uism/plugins
   {:valid-credentials? (fn [{:keys [username password]}]
                          (and (seq username) (seq password)))}

   ::uism/states
   {:initial
    {::uism/events {::uism/started {::uism/target-state :filling-info
                                    ::uism/handler      (fn [{::uism/keys [event-data] :as env}]
                                                          (if (uism/alias-value env :logged-in?)
                                                            ;; An example of the handler overriding the target state
                                                            (do
                                                              (js/alert "You're already logged in!")
                                                              (uism/exit env))
                                                            (-> env
                                                              (uism/load ::session
                                                                (uism/actor-class env :session)
                                                                {:abort-id         :abort/session-load
                                                                 ::uism/post-event :session-checked})
                                                              (uism/set-timeout :timer/session-load :event/session-load-timeout {} 1000 #{:session-checked})
                                                              (uism/set-aliased-value
                                                                :visible? true
                                                                :login-enabled? false
                                                                :username ""
                                                                :password "")
                                                              (cond-> (:slow? event-data)
                                                                (uism/store :slow? true)))))}}}

    :filling-info
    {::uism/events {:event/session-load-timeout {::uism/handler (fn [env]
                                                                  (log/debug "Aborting session load")
                                                                  (when @my-app
                                                                    (fc/abort-request! @my-app :abort/session-load))
                                                                  env)}
                    ::uism/value-changed        {::uism/handler
                                                 (fn [env]
                                                   (let [valid?   (uism/run env :valid-credentials?)
                                                         enabled? (uism/alias-value env :login-enabled?)]
                                                     (if (not= valid? enabled?)
                                                       (uism/set-aliased-value env :login-enabled? valid?))))}

                    :login!                     {::uism/event-predicate
                                                 (fn [env] (uism/run env :valid-credentials?))

                                                 ::uism/handler
                                                 (fn [{::uism/keys [event-data] :as env}]
                                                   (-> env
                                                     (uism/set-aliased-value :login-enabled? false :error "" :busy? true)
                                                     ;; set a 2s timeout that auto-cancels on success or failure events
                                                     (uism/set-timeout :timer/login :login-timed-out! {} 2000 #{:success :failure})
                                                     (uism/trigger-remote-mutation :form `login
                                                       (merge event-data
                                                         {::uism/ok-event        :success
                                                          ::uism/error-event     :failure
                                                          ::uism/mutation-remote (if (uism/retrieve env :slow?) :slow-remote :remote)
                                                          ::pm/returning         (uism/actor-class env :session)
                                                          ::pm/target            [::session]}))))

                                                 ::uism/target-state
                                                 :attempting-login}

                    :session-checked            {::uism/handler (fn [{::uism/keys [event-data] :as env}]
                                                                  (log/info "Load complete" event-data)
                                                                  (let [logged-in? (uism/alias-value env :logged-in?)]
                                                                    (when logged-in?
                                                                      (js/alert "The server indicated you were already logged in.")
                                                                      (-> env
                                                                        (uism/set-aliased-value :visible? false)
                                                                        (uism/exit)))))}}}

    :attempting-login
    {::uism/events {::uism/value-changed {::uism/event-predicate (fn [env] false)
                                          ::uism/handler         identity}

                    :login-timed-out!    {::uism/target-state ::uism/exit
                                          ::uism/handler      (fn [env]
                                                                (js/alert "Server didn't respond in time!")
                                                                (-> env
                                                                  (uism/set-aliased-value
                                                                    :visible? false
                                                                    :busy? false)))}

                    :success             {::uism/target-state ::uism/exit
                                          ::uism/handler      (fn [env]
                                                                (-> env
                                                                  (uism/set-aliased-value :busy? false :visible? false)))}

                    :failure             {::uism/target-state :filling-info
                                          ::uism/handler      (fn [{::uism/keys [event-data] :as env}]
                                                                (log/info "Server error was " (::pm/mutation-errors event-data))
                                                                (-> env
                                                                  (uism/set-aliased-value
                                                                    :error "Invalid credentials. Please try again."
                                                                    :busy? false)))}}}}})

(defsc LoginForm [this {:keys [ui/login-enabled? ui/login-error ui/busy? user/email user/password] :as props}]
  {:query         [:ui/login-enabled? :ui/login-error :ui/busy? :user/email :user/password]
   :ident         (fn [] [:COMPONENT/by-id ::login])
   :initial-state {:user/email "" :user/password ""}}
  (let [error?        (seq login-error)
        error-classes [(when error? "error")]
        login!        (fn [] (uism/trigger! this ::loginsm :login! {:username email :password password}))]
    (dom/div :.ui.container.form {:classes (into error-classes [(when busy? "loading")])}
      (dom/div :.field {:classes error-classes}
        (dom/label "Email")
        (dom/input {:value     email
                    :disabled  busy?
                    :onKeyDown (fn [evt] (when (evt/enter? evt) (login!)))
                    :onChange  (fn [evt] (uism/set-string! this ::loginsm :username evt))}))
      (dom/div :.field {:classes error-classes}
        (dom/label "Password")
        (dom/input {:value     password
                    :disabled  busy?
                    :type      "password"
                    :onKeyDown (fn [evt]
                                 (when (evt/enter? evt) (login!)))
                    :onChange  (fn [evt] (uism/set-string! this ::loginsm :password evt))}))
      (dom/div :.field
        (dom/button {:disabled (not login-enabled?)
                     :onClick  (fn [] (login!))}
          "Login"))
      (when error?
        (dom/div :.ui.error.message
          (dom/p login-error))))))

(def ui-login-form (prim/factory LoginForm {:keyfn :db/id}))

(defsc Dialog [this {:keys [ui/active? dialog/form]}]
  {:query         [:ui/active? {:dialog/form (prim/get-query LoginForm)}]
   :ident         (fn [] [:COMPONENT/by-id ::dialog])
   :initial-state {:ui/active? false :dialog/form {}}}
  (sui/ui-modal {:open    active? :closeIcon true :closeOnDimmerClick true :closeOnDocumentClick true
                 :onClose (fn [] (uism/trigger! this ::loginsm ::uism/exit))}
    (sui/ui-modal-header {} "Login")
    (sui/ui-modal-content {}
      (ui-login-form form))))

(def ui-dialog (prim/factory Dialog {:keyfn :ui/active?}))

(defmutation logout [_]
  (action [{:keys [state]}]
    (swap! state update :globals assoc ::session {:logged-in? false}))
  (remote [_] true))

(defsc Root [this {:keys [root/dialog]}]
  {:query         [{:root/dialog (prim/get-query Dialog)}]
   :initial-state {:root/dialog {}}}
  (dom/div nil
    (dom/button {:onClick #(prim/transact! this `[(logout {})])}
      "Log Out")
    (dom/button
      {:onClick (fn []
                  (uism/begin! this login-machine ::loginsm {:dialog  Dialog
                                                             :session Session
                                                             :form    LoginForm}))}
      "Login")
    (ui-dialog dialog)))

(defonce server-session (atom {:logged-in? false}))

(server/defquery-root ::session
  (value [env params]
    @server-session))

;; ================================================================================
;; Simulated server:  Returns valid session if the username is "tony"
;; ================================================================================

(server/defmutation logout [_]
  (action [_]
    (reset! server-session {:logged-in? false})))

(server/defmutation login [{:keys [username password]}]
  (action [_]
    (if (= username "tony")
      (do
        (reset! server-session {:logged-in? true
                                :username   "Tony"})
        @server-session)
      {:logged-in?          false
       ::pm/mutation-errors "No way man!"})))

(ws/defcard state-machine-demo-card
  {::wsm/card-width  4
   ::wsm/align       {:flex 1}
   ::wsm/card-height 4}
  (ct.fulcro/fulcro-card
    {::f.portal/root       Root
     ::f.portal/wrap-root? false
     ::f.portal/app        {:started-callback (fn [app] (reset! my-app app))
                            :networking       {:remote (server/new-server-emulator (server/fulcro-parser) 100)}}}))
