(ns fulcro.incubator.ui-state-machines-test
  (:require
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [fulcro-spec.core :refer [specification provided provided! when-mocking when-mocking! behavior assertions component]]
    [fulcro.client.data-fetch :as df]
    [fulcro.client.impl.data-targeting :as dft]
    [fulcro.client.impl.protocols :as fcip]
    [fulcro.client.mutations :as m]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.incubator.ui-state-machines :as uism]
    [fulcro.incubator.test-helpers :as th]
    [taoensso.timbre :as log]))

(declare => =1x=>)

(defsc AClass [_ _] {:query ['*] :ident (fn [] [:A 1])})
(defn A-handler [env] env)
(uism/defstatemachine test-machine
  {::uism/actor-names #{:dialog}
   ::uism/aliases     {:visible? [:dialog :ui/active?]
                       :title    [:dialog :title]
                       :username [:dialog :name]}
   ::uism/plugins     {:f? (fn [{:keys [visible? username]}]
                             (when visible? username))}
   ::uism/states      {:initial {::uism/handler (fn [env]
                                                  (with-meta env {:handler-ran true}))}

                       :B       {::uism/events {:bang! {::uism/handler         (fn [env] (uism/store env :handler-ran? true))
                                                        ::uism/target-state    :A
                                                        ::uism/event-predicate (fn [env] (uism/retrieve env :enabled? false))}}}
                       :C       {::uism/events {:bang! {::uism/handler      (fn [env] (uism/store env :handler-ran? true))
                                                        ::uism/target-state :A}}}
                       :D       {::uism/events {:bang! {::uism/handler      (fn [env] (uism/activate env :A))
                                                        ::uism/target-state :B}}}
                       :A       {::uism/handler A-handler}}})

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
    (uism/lookup-state-machine-field (test-env :event nil) ::uism/actor-names) => #{:dialog}))

(specification "state-machine-env"
  (assertions
    "produces a spec-compliant result"
    (s/valid? ::uism/env (uism/state-machine-env {} [:a 1] :x :evt {})) => true))

(let [env (test-env nil nil)]
  (specification "asm-value"
    (assertions "Finds things in the active state machine"
      (uism/asm-value env ::uism/asm-id) => :fake))

  (specification "activate"
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

  (specification "store/retrieve"
    (assertions
      "Allows for the local storage or asm-local values"
      (-> env (uism/store :x true) (uism/retrieve :x)) => true
      (-> env (uism/store :x 0) (uism/retrieve :x)) => 0
      (-> env (uism/store :x {}) (uism/retrieve :x)) => {}
      (-> env (uism/store :v 1) (uism/retrieve :v)) => 1))

  (specification "resolve-alias"
    (assertions
      "Returns the Fulcro state path for a given data alias"
      (uism/resolve-alias env :username) => [:TABLE 1 :name])

    (assertions
      "Returns nil if it is an invalid alias"
      (uism/resolve-alias env :crap) => nil))

  (specification "actor-path"
    (assertions
      "Returns the Fulcro ident of an actor"
      (uism/actor-path env :dialog) => [:TABLE 1]
      "Returns the Fulcro path to data in an actor if a field is included"
      (uism/actor-path env :dialog :boo) => [:TABLE 1 :boo]))

  (specification "set-actor-value"
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

  (specification "alias-value"
    (when-mocking
      (log/-log! _ _ _ _ _ _ _ msg _ _) =1x=> (assertions "Logs an error"
                                                (force msg) => ["Unable to find alias in state machine:" :name])

      (assertions
        "Returns nil if the alias isn't valid"
        (uism/alias-value env :name) => nil
        "Gets the value of the fulro state that the alias refers to"
        (uism/alias-value env :username) => "Joe")))

  (specification "set-aliased-value"
    (assertions
      "Can set a single value"
      (-> env
        (uism/set-aliased-value :title "Hello")
        (uism/alias-value :title)) => "Hello"
      "Can set two values"
      (-> env
        (uism/set-aliased-value :title "Hello" :username "Joe")
        (uism/alias-value :title)) => "Hello"
      (-> env
        (uism/set-aliased-value :title "Hello" :username "Joe")
        (uism/alias-value :username)) => "Joe"
      "Can set more than 2 values"
      (-> env
        (uism/set-aliased-value :title "Hello" :username "Joe" :visible? :booga)
        (uism/alias-value :visible?)) => :booga)

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
          :name) => "Sam")))

  (specification "aliased-data"
    (assertions
      "Builds a map of all of the current values of aliased data"
      (-> env
        (uism/aliased-data)) => {:visible? false
                                 :title    nil
                                 :username "Joe"}))

  (specification "run"
    (assertions
      "Runs a plugin against the env"
      (-> env
        (uism/set-aliased-value :visible? true)
        (uism/run :f?)) => "Joe"

      "Can be passed extra data that can overwrite aliased values"
      (-> env
        (uism/set-aliased-value :visible? true)
        (uism/run :f? {:visible? false})) => nil))

  (specification "active-state-handler"
    (provided! "The state machine definition is missing or the active state has no handler"
      (uism/lookup-state-machine e) => (do
                                         (assertions
                                           e => env)
                                         test-machine)
      (uism/asm-value e k) => (do
                                (assertions
                                  e => env
                                  k => ::uism/active-state)
                                :boo)

      (log/-log! _ _ _ _ _ _ _ msg _ _) =1x=> (assertions "Logs an error"
                                                (force msg) => ["UNEXPECTED EVENT: Did not find a way to handle event" nil "in the current active state:" :boo])

      (assertions
        "returns core identity"
        (uism/active-state-handler env) => identity))
    (let [env     (-> (test-env :bang! nil) (uism/activate :B) (uism/store :enabled? true))
          handler (fn geh* [e] e)]
      (provided! "The state machine definition has event definitions"
        (uism/lookup-state-machine e) => (do
                                           (assertions
                                             e => env)
                                           test-machine)
        (uism/asm-value e k) => (do
                                  (assertions
                                    e => env
                                    k => ::uism/active-state)
                                  :boo)

        (uism/generic-event-handler e) => handler

        (assertions
          "returns a generic-event-handler"
          (uism/active-state-handler env) => handler)))
    (provided! "The state machine definition exists and there is an active state."
      (uism/lookup-state-machine e) => (do
                                         (assertions
                                           e => env)
                                         test-machine)
      (uism/asm-value e k) => (do
                                (assertions
                                  e => env
                                  k => ::uism/active-state)
                                :A)

      (assertions
        "returns the correct handler"
        (uism/active-state-handler env) => A-handler)))

  (specification "generic-event-handler"
    (let [disabled-env              (-> (test-env :bang! nil) (uism/activate :B) (uism/store :enabled? false))
          enabled-env               (-> disabled-env (uism/store :enabled? true))
          non-event-env             (-> (test-env :boo nil) (uism/activate :A))
          missing-handler           (uism/generic-event-handler non-event-env)
          disabled-handler          (uism/generic-event-handler disabled-env)
          enabled-handler           (uism/generic-event-handler enabled-env)
          no-predicate-env          (-> enabled-env (uism/activate :C))
          no-predicate-handler      (uism/generic-event-handler no-predicate-env)
          overriding-handler-env    (-> disabled-env (uism/activate :D))
          overriding-handler        (uism/generic-event-handler overriding-handler-env)

          actual-disabled-result    (disabled-handler disabled-env)
          actual-enabled-result     (enabled-handler enabled-env)
          no-predicate-result       (no-predicate-handler no-predicate-env)
          overriding-handler-result (overriding-handler overriding-handler-env)]

      (behavior "when the event definition is missing"
        (assertions
          "Returns nil"
          missing-handler => nil))

      (behavior "When there is no predicate"
        (assertions
          "Runs the handler"
          (-> no-predicate-result (uism/retrieve :handler-ran? false)) => true
          "Follows the transition, if defined"
          (-> no-predicate-result (uism/asm-value ::uism/active-state)) => :A))
      (behavior "when the predicate is false"
        (assertions
          "Does not runs the handler"
          (-> actual-disabled-result (uism/retrieve :handler-ran? false)) => false
          "Stays in the same state"
          (-> actual-disabled-result (uism/asm-value ::uism/active-state)) => :B))
      (behavior "when the predicate is true"
        (assertions
          "Runs the handler"
          (-> actual-enabled-result (uism/retrieve :handler-ran? false)) => true
          "Follows the transition, if defined"
          (-> actual-enabled-result (uism/asm-value ::uism/active-state)) => :A))
      (behavior "when the handler activates a target state different from the *declared* target state"
        (assertions
          "The handler wins"
          (-> overriding-handler-result (uism/asm-value ::uism/active-state)) => :A))))

  (specification "apply-event-value"
    (assertions
      "returns an unmodified env for other events"
      (uism/apply-event-value env {::uism/event-id :random-event}) => env
      "applies a change to fulcro state based on the ::value-changed event"
      (-> env
        (uism/apply-event-value {::uism/event-id   ::uism/value-changed
                                 ::uism/event-data {::uism/alias :visible?
                                                    :value       :new-value}})
        (uism/alias-value :visible?)) => :new-value))

  (specification "queue-mutations!"
    (let [mutation-1-descriptor {::uism/mutation-context :dialog
                                 ::uism/ok-event         :pow!
                                 ::uism/mutation         `a}
          mutation-2-descriptor {::uism/mutation-context :dialog
                                 ::uism/ok-event         :bam!
                                 ::uism/mutation         `b}
          menv1                 (assoc env ::uism/queued-mutations [mutation-1-descriptor])
          menv                  (assoc env ::uism/queued-mutations [mutation-1-descriptor
                                                                    mutation-2-descriptor])
          mock-component        #js {:fulcro$isComponent true}]
      (behavior "Walks the list of queued mutations in env"
        (when-mocking!
          (uism/actor->ident e actor) =2x=> [:actor 1]
          (prim/ref->any r i) =2x=> mock-component
          (pm/pmutate! comp m params)
          =1x=> (assertions
                  "Calls pmutate with the (1st) mutation delegate and the mutation descriptor "
                  m => uism/mutation-delegate
                  params => mutation-1-descriptor)
          (pm/pmutate! comp m params)
          =1x=> (assertions
                  "Calls pmutate with the (2nd) mutation delegate and the mutation descriptor "
                  m => uism/mutation-delegate
                  params => mutation-2-descriptor)

          (uism/queue-mutations! (prim/reconciler {}) menv)))
      (behavior "When it cannot find a contextual component:"
        (when-mocking!
          (uism/actor->ident e actor) =1x=> [:actor 1]
          (prim/ref->any r i) =1x=> nil
          (log/-log! _ _ _ _ _ _ _ msg _ _) =1x=> (assertions "Logs an error"
                                                    (first (force msg)) =fn=> #(str/includes? % "Cannot run"))

          (uism/queue-mutations! (prim/reconciler {}) menv1)))))

  (specification "queue-actor-load!"
    (let [reconciler   (prim/reconciler {})
          env          (assoc env ::uism/queued-loads [])
          load-called? (atom false)]
      (when-mocking!
        (uism/actor->ident e actor) =1x=> [:actor 1]
        (df/load r ident class params) =1x=> (do
                                               (reset! load-called? true)
                                               (assertions
                                                 "Sends a real load to fulcro containing: the proper actor ident"
                                                 ident => [:actor 1]
                                                 "the correct component class"
                                                 class => AClass
                                                 "The params with specified load options"
                                                 params => {:marker false})
                                               true)
        (uism/defer f) => (do
                            (assertions "Defers the actual load to a lambda"
                              @load-called? => false)
                            (f)
                            (assertions
                              @load-called? => true))

        (uism/queue-actor-load! reconciler env :dialog AClass {:marker false}))))

  #?(:cljs
     (let [fulcro-state (atom {})
           mutation-env {:state fulcro-state :ref [:TABLE 1] :reconciler (prim/reconciler {})}
           event        {::uism/event-id :boggle ::uism/asm-id :fake}]
       (specification "trigger-state-machine-event!"
         (let [handler    (fn [env] (assoc env :handler-ran true))
               final-env? (fn [env]
                            (assertions
                              "Receives the final env"
                              (:looked-up-env env) => true
                              (:applied-event-values env) => true
                              (:handler-ran env) => true))]
           (when-mocking!
             (uism/state-machine-env s r a e d) => (assoc env :looked-up-env true)
             (uism/clear-timeouts-on-event! env event) => (do
                                                            (assertions
                                                              "Clears any auto-cleared timeouts"
                                                              true => true)
                                                            (assoc env :cleared-timeouts event))
             (uism/active-state-handler e) => handler
             (uism/apply-event-value env evt) => (assoc env :applied-event-values true)

             ;; not calling the mocks on these will cause fail. our assertion is just
             ;; for pos output
             (uism/queue-mutations! r e) => (do (final-env? e)
                                                (assertions
                                                  "Tries to queue mutations using the final env"
                                                  true => true)
                                                nil)

             (uism/queue-loads! r env) => (do (final-env? env)
                                              (assertions
                                                "Tries to queue loads using the final env"
                                                true => true)
                                              nil)

             (uism/update-fulcro-state! env satom) => (do (final-env? env)
                                                          (assertions
                                                            "Tries to update fulcro state"
                                                            satom => fulcro-state)
                                                          nil)

             (uism/ui-refresh-list env) => [:x :y]

             ;; ACTION UNDER TEST
             (let [actual (uism/trigger-state-machine-event! mutation-env event)]
               (assertions
                 "returns the list of things to refresh in the UI"
                 actual => [:x :y])))))))

  (specification "trigger-state-machine-event mutation"
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

  (specification "set-string!"
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

         (uism/set-string! {} :fake :username #js {:target #js {:value "hi"}}))))

  (specification "derive-actor-idents" :focused
    (let [actual (uism/derive-actor-idents {:a [:x 1]
                                            :b AClass
                                            ; :c (th/mock-component AClass {})
                                            :d (uism/with-actor-class [:A 1] AClass)})]
      (assertions
        "allows a bare ident"
        (:a actual) => [:x 1]
        "finds the ident on singleton classes"
        (:b actual) => [:A 1]
        "remembers the singleton class as metadata"
        (:b actual) => [:A 1]
        (-> actual :b meta ::uism/class) => AClass
        ;; Need enzyme configured consistently for this test
        ;"remembers the class of a react instance"
        ;(:c actual) => [:A 1]
        ;(-> actual :c meta ::uism/class) => AClass
        "remembers an explicity 'with'"
        (-> actual :d meta ::uism/class) => AClass)))
  (specification "set-timeout"
    (let [new-env    (uism/set-timeout env :timer/my-timer :event/bam! {} 100)
          descriptor (some-> new-env ::uism/queued-timeouts first)]
      (assertions
        "Adds a timeout descriptor to the queued timeouts"
        (s/valid? ::uism/timeout-descriptor descriptor) => true
        "whose default auto-cancel is constantly false"
        (some-> descriptor ::uism/cancel-on meta :cancel-on (apply [:x])) => false)))

  (let [prior-timer           {::uism/timeout   100
                               ::uism/timer-id  :timer/id
                               ::uism/js-timer  (with-meta {} {:timer :mock-js-timer})
                               ::uism/event-id  :bam!
                               ::uism/cancel-on (with-meta {} {:cancel-on (constantly true)})}
        env-with-active-timer (assoc-in env (uism/asm-path env [::uism/active-timers :timer/id]) prior-timer)]
    (specification "schedule-timeouts!"
      (provided! "There isn't already a timer under that ID"
        (uism/get-js-timer e t) =1x=> nil
        (uism/set-js-timeout! f t) =1x=> (assertions
                                           "sets the low-level timer with the correct time"
                                           t => 100)

        (let [prepped-env (uism/set-timeout env :timer/id :bam! {} 100)
              new-env     (uism/schedule-timeouts! (prim/reconciler {}) prepped-env)]

          (assertions
            "Adds a timeout descriptor to active timers"
            (s/valid? ::uism/timeout-descriptor
              (get-in new-env (uism/asm-path new-env [::uism/active-timers :timer/id])))
            => true)))

      (provided! "There IS a timer under that ID"
        (uism/get-js-timer e t) =1x=> :low-level-js-timer
        (uism/clear-js-timeout! t) =1x=> (assertions
                                           "Clears the old timer"
                                           t => :low-level-js-timer)
        (uism/set-js-timeout! f t) =1x=> (assertions
                                           "sets the low-level timer with the correct time"
                                           t => 300)
        (let [prepped-env (uism/set-timeout env-with-active-timer :timer/id :bam! {} 300)
              new-env     (uism/schedule-timeouts! (prim/reconciler {}) prepped-env)]
          (assertions
            "Adds a timeout descriptor to active timers"
            (s/valid? ::uism/timeout-descriptor
              (get-in new-env (uism/asm-path new-env [::uism/active-timers :timer/id])))
            => true))))

    (specification "clear-timeout!"
      (provided! "The timer exists"
        (uism/clear-js-timeout! t) => (assertions
                                        "clears the timer"
                                        t => :mock-js-timer)

        (let [new-env (uism/clear-timeout! env-with-active-timer :timer/id)]
          (assertions
            "Removes the timer from the active timers table"
            (get-in new-env (uism/asm-path new-env [::uism/active-timers :timer/id])) => nil))))

    (specification "clear-timeouts-on-event!"
      (provided! "clear-timeout! works correctly"
        (uism/clear-timeout! e t) => (assoc e :cleared? true)

        (let [new-env (uism/clear-timeouts-on-event! env-with-active-timer :bam!)]
          (assertions
            "Clears and removes the timer"
            (:cleared? new-env) => true))))))

;; not usable from clj
(specification "begin!"
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
    #?(:cljs
       (component "(the wrapper function begin!)"
         (when-mocking
           (prim/transact! t tx) => (assertions
                                      "runs fulcro transact on the begin mutation"
                                      (ffirst tx) => `uism/begin)


           (uism/begin! (prim/reconciler {}) test-machine :fake {:dialog [:table 1]}))))))

(uism/defstatemachine ctm {::uism/aliases     {:x [:dialog :foo]}
                           ::uism/actor-names #{:dialog}
                           ::uism/states      {:initial {::uism/handler (fn [env] env)}}})
(specification "compute-target"
  (let [asm      (uism/new-asm {::uism/state-machine-id `ctm ::uism/asm-id :fake ::uism/actor->ident {:dialog [:dialog 1]}})
        test-env (uism/state-machine-env {::uism/asm-id {:fake asm}} nil :fake :do {})]
    (behavior "accepts (and returns) any kind of raw fulcro target"
      (assertions
        "(normal target)"
        (uism/compute-target test-env {::pm/target [:a 1]}) => [:a 1]
        "(speecial target)"
        (uism/compute-target test-env {::pm/target (df/append-to [:a 1])}) => [:a 1]
        (dft/special-target? (uism/compute-target test-env {::pm/target (df/append-to [:a 1])})) => true))
    (behavior "Resolves actors"
      (assertions
        (uism/compute-target test-env {::uism/target-actor :dialog}) => [:dialog 1]
        "can combine plain targets with actor targets"
        (uism/compute-target test-env {::pm/target [:a 1] ::uism/target-actor :dialog}) => [[:a 1] [:dialog 1]]
        (dft/multiple-targets? (uism/compute-target test-env {::pm/target         [:a 1]
                                                              ::uism/target-actor :dialog}))
        => true

        "can combine actor targets with a multiple-target"
        (uism/compute-target test-env {::pm/target         (df/multiple-targets [:a 1] [:b 2])
                                       ::uism/target-actor :dialog})
        => [[:a 1] [:b 2] [:dialog 1]]))
    (behavior "Resolves aliases"
      (assertions
        (uism/compute-target test-env {::uism/target-alias :x}) => [:dialog 1 :foo]
        "can combine plain targets with alias targets"
        (uism/compute-target test-env {::pm/target [:a 1] ::uism/target-alias :x}) => [[:a 1] [:dialog 1 :foo]]
        (dft/multiple-targets? (uism/compute-target test-env {::pm/target         [:a 1]
                                                              ::uism/target-alias :x}))
        => true

        "can combine alias targets with a multiple-target"
        (uism/compute-target test-env {::pm/target         (df/multiple-targets [:a 1] [:b 2])
                                       ::uism/target-alias :x})
        => [[:a 1] [:b 2] [:dialog 1 :foo]]))))

