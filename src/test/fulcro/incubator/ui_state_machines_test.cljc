(ns fulcro.incubator.ui-state-machines-test
  (:require
    [fulcro.incubator.ui-state-machines :as uism]
    [fulcro.client.mutations :as m]
    [fulcro.client.primitives :as prim]
    [fulcro.client.impl.protocols :as fcip]
    [fulcro-spec.core :refer [specification provided provided! when-mocking when-mocking! behavior assertions component]]
    [clojure.spec.alpha :as s]
    [taoensso.timbre :as log]))

(uism/defstatemachine test-machine
  {::uism/actor-names #{:dialog}
   ::uism/aliases     {:visible? [:dialog :ui/active?]
                       :username [:dialog :name]}
   ::uism/plugins     {:f? (fn [{:keys [visible? username]}]
                             (when visible? username))}
   ::uism/states      {:initial {::uism/handler (fn [env]
                                                  (with-meta env {:handler-ran true}))}

                       :A       {::uism/handler (fn [env] env)}}})

(def base-fulcro-state {:TABLE        {1 {:ui/active? false :name "Joe"}}
                        ::uism/asm-id {:fake (uism/new-asm {::uism/state-machine-id `test-machine
                                                            ::uism/asm-id           :fake
                                                            ::uism/actor->ident     {:dialog [:TABLE 1]}})}})
(defn test-env [event-id event-data] (uism/state-machine-env base-fulcro-state [:TABLE 1] :fake event-id event-data))

(specification "State Machine Registry"
  (assertions
    "Registers the FQ symbol of the state machine"
    (map? (uism/get-state-machine `test-machine)) => true
    "Stores the definition at that symbol"
    (::uism/actor-names (uism/get-state-machine `test-machine)) => #{:dialog}
    "Allows lookup of a value via an active env and a machine key"
    (uism/lookup-state-machine-field (test-env nil nil) ::uism/actor-names) => #{:dialog}))


(specification "Basic State Machine Operations"
  (let [env (test-env nil nil)]
    (component "asm-value"
      (assertions "Finds things in the active state machine"
        (uism/asm-value env ::uism/asm-id) => :fake))

    (component "activate"
      (assertions
        "Sets the given active state in the env"
        (-> env
          (uism/activate :A)
          (uism/asm-value ::uism/active-state)) => :A)
      (when-mocking
        (log/-log! _ _ _ _ _ _ _ msg _ _) =1x=> (assertions "Logs an error"
                                                  (force msg) => ["Activate called for invalid state: " :crap])

        (assertions
          "Ignores a requst to move to an invalid state (logs an error)"
          (-> env
            (uism/activate :crap)
            (uism/asm-value ::uism/active-state)) => :initial)))

    (component "store/retrieve"
      (assertions
        "Allows for the local storage or asm-local values"
        (-> env
          (uism/store :v 1)
          (uism/retrieve :v)) => 1))

    (component "resolve-alias"
      (assertions
        "Returns the Fulcro state path for a given data alias"
        (uism/resolve-alias env :username) => [:TABLE 1 :name])

      (assertions
        "Returns nil if it is an invalid alias"
        (uism/resolve-alias env :crap) => nil))

    (component "actor-path"
      (assertions
        "Returns the Fulcro ident of an actor"
        (uism/actor-path env :dialog) => [:TABLE 1]
        "Returns the Fulcro path to data in an actor if a field is included"
        (uism/actor-path env :dialog :boo) => [:TABLE 1 :boo]))

    (component "set-actor-value"
      (assertions
        "Sets a raw (non-aliased) attribute in Fulcro state on an actor"
        (-> env
          (uism/set-actor-value :dialog :boo 42)
          ::uism/state-map
          :TABLE
          (get 1)
          :boo) => 42

        "Which can be read by actor-value"
        (-> env
          (uism/set-actor-value :dialog :boo 42)
          (uism/actor-value :dialog :boo)) => 42))

    (component "alias-value"
      (when-mocking
        (log/-log! _ _ _ _ _ _ _ msg _ _) =1x=> (assertions "Logs an error"
                                                  (force msg) => ["Unable to find alias in state machine:" :name])

        (assertions
          "Returns nil if the alias isn't valid"
          (uism/alias-value env :name) => nil
          "Gets the value of the fulro state that the alias refers to"
          (uism/alias-value env :username) => "Joe")))

    (component "set-aliased-value"
      (when-mocking
        (log/-log! _ _ _ _ _ _ _ msg _ _) =1x=> (assertions "Logs an error"
                                                  (force msg) => ["Attempt to set a value on an invalid alias:" :name])

        (assertions
          "Returns unmodified env if the alias isn't valid"
          (uism/set-aliased-value env :name "Sam") => env
          "Sets the value in the fulro state that the alias refers to"
          (-> env
            (uism/set-aliased-value :username "Sam")
            ::uism/state-map
            :TABLE
            (get 1)
            :name) => "Sam"
          )))

    (component "aliased-data"
      (assertions
        "Builds a map of all of the current values of aliased data"
        (-> env
          (uism/aliased-data)) => {:visible? false
                                   :username "Joe"}))

    (component "run"
      (assertions
        "Runs a plugin against the env"
        (-> env
          (uism/set-aliased-value :visible? true)
          (uism/run :f?)) => "Joe"

        "Can be passed extra data that can overwrite aliased values"
        (-> env
          (uism/set-aliased-value :visible? true)
          (uism/run :f? {:visible? false})) => nil))

    (component "apply-event-value"
      (assertions
        "returns an unmodified env for other events"
        (uism/apply-event-value env {::uism/event-id :random-event}) => env
        "applies a change to fulcro state based on the ::value-changed event"
        (-> env
          (uism/apply-event-value {::uism/event-id   ::uism/value-changed
                                   ::uism/event-data {::uism/alias :visible?
                                                      :value       :new-value}})
          (uism/alias-value :visible?)) => :new-value))

    (let [fulcro-state (atom {})
          mutation-env {:state fulcro-state :ref [:TABLE 1]}
          event        {::uism/event-id :boggle ::uism/asm-id :fake}]
      (component "trigger-state-machine-event!"
        (when-mocking
          (uism/lookup-state-machine env) => (do (assertions
                                                   "Generates a value environment for the invocation"
                                                   (s/valid? ::uism/env env) => true
                                                   "Looks up the state machine definition" ; the fact that this is invoked is proof, but want output in spec
                                                   true => true)
                                                 test-machine)
          (uism/actor->ident e k) =1x=> (do
                                          (assertions
                                            "looks up the actors for ui refresh"
                                            k => :dialog)
                                          :x)
          (uism/asm-value e k) =1x=> (do
                                       (assertions
                                         "Looks up the current active state"
                                         k => ::uism/active-state)
                                       :initial)
          (uism/apply-event-value e p) =1x=> (do
                                               (assertions
                                                 "Attempts to apply value-changed events"
                                                 p => event)
                                               (assoc e ::uism/state-map {:modified true}))
          (uism/asm-value final-env k) =1x=> (do
                                               (assertions
                                                 "Runs the handler on the env (inferred because passed to next stage)"
                                                 (:handler-ran (meta final-env)) => true)
                                               :A)


          (uism/trigger-state-machine-event! mutation-env event)

          (assertions
            "Puts the new state into fulcro"
            @fulcro-state => {:modified true}))))

    (component "trigger-state-machine-event mutation"
      (let [{:keys [action]} (m/mutate {:reconciler :r} `uism/trigger-state-machine-event {:params true})]
        (when-mocking!
          (uism/trigger-state-machine-event! mutation-env p) => (do
                                                                  (assertions
                                                                    "runs the state machine event"
                                                                    mutation-env => {:reconciler :r}
                                                                    p => {:params true})
                                                                  [[:table 1]])
          (fcip/queue! r items) => (do
                                     (assertions
                                       "Queues the actors for UI refresh"
                                       items => [[:table 1]]))

          (action))))

    (component "set-string!"
      (provided! "The user supplies a string"
        (uism/trigger! this smid event params) => (do
                                                    (assertions
                                                      "Triggers a value-changed event"
                                                      event => ::uism/value-changed
                                                      "on a named state machine"
                                                      smid => :fake
                                                      "whose parameters specify the new value"
                                                      (::uism/alias params) => :username
                                                      (:value params) => "hello")
                                                    nil)

        (uism/set-string! {} :fake :username "hello"))
      #?(:cljs
         (provided! "The user supplies a js DOM onChange event"
           (uism/trigger! this smid event params) => (do
                                                       (assertions
                                                         "Triggers a value-changed event"
                                                         event => ::uism/value-changed
                                                         "on a named state machine"
                                                         smid => :fake
                                                         "with the extracted string value"
                                                         (::uism/alias params) => :username
                                                         (:value params) => "hi")
                                                       nil)

           (uism/set-string! {} :fake :username #js {:target #js {:value "hi"}}))))))

(specification "begin!" :focused
  (let [fulcro-state (atom {})
        mutation-env {:state fulcro-state :ref [:TABLE 1]}]
    (component "(the begin mutation)"
      (let [creation-args {::uism/state-machine-id `test-machine
                           ::uism/asm-id           :fake
                           ::uism/actor->ident     {:dialog [:table 1]}}
            {:keys [action]} (m/mutate mutation-env `uism/begin creation-args)
            real-new-asm  uism/new-asm]
        (when-mocking!
          (uism/new-asm p) =1x=> (do
                                   (assertions
                                     "creates a new asm with the provided params"
                                     p => creation-args)
                                   (real-new-asm p))
          (uism/trigger-state-machine-event! e p) =1x=> (do
                                                          (assertions
                                                            "triggers the ::started event"
                                                            (::uism/event-id p) => ::uism/started
                                                            (::uism/asm-id p) => :fake)
                                                          nil)

          (action)

          (assertions
            "and stores it in fulcro state"
            (contains? (::uism/asm-id @fulcro-state) :fake) => true))))
    (component "(the wrapper function begin!)"
      (when-mocking
        (prim/transact! t tx) => (assertions
                                   "runs fulcro transact on the begin mutation"
                                   (ffirst tx) => `uism/begin
                                   )

        (uism/begin! (prim/reconciler {}) test-machine :fake {:dialog [:table 1]})))))
