(ns fulcro.incubator.ui-state-machines
  #?(:cljs (:require-macros [fulcro.incubator.ui-state-machines]))
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [fulcro.client.data-fetch :as df]
    [fulcro.client.impl.data-targeting :as dft]
    [fulcro.client.impl.protocols :as fcip]
    [fulcro.client.mutations :as m :refer [defmutation]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.incubator.mutation-interface :as mi]
    [fulcro.incubator.spec-helpers :refer [Defn =>]]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.util :as futil]
    [taoensso.timbre :as log]))

(mi/declare-mutation mutation-delegate `mutation-delegate)

;; Active State Machine and ENV specs
(s/def ::state-map map?)
(s/def ::fulcro-ident (s/with-gen futil/ident? #(s/gen #{[:table 1] [:other :tab]})))
(s/def ::fulcro-reconciler (s/with-gen prim/reconciler? #(s/gen #{(prim/reconciler {})})))
(s/def ::source-actor-ident ::fulcro-ident)
(s/def ::actor-name keyword?)
(s/def ::actor->ident (s/map-of ::actor-name ::fulcro-ident))
(s/def ::ident->actor (s/map-of ::fulcro-ident ::actor-name))
(s/def ::active-state keyword?)                             ; The state the active instance is currently in
(s/def ::state-machine-id (s/with-gen symbol? #(s/gen #{'the-state-machine}))) ; The symbol of the state machine's definition
(s/def ::asm-id any?)                                       ; The ID of the active instance in fulcro state
(s/def ::local-storage (s/map-of keyword? any?))
;; aliases and plugins are copied from the definition
(s/def ::asm (s/keys :req [::asm-id ::state-machine-id ::active-state ::actor->ident ::ident->actor]
               :opt [::local-storage]))
(s/def ::state-id keyword?)
(s/def ::event-data map?)
(s/def ::event-id keyword?)
(s/def ::env (s/keys :req [::state-map ::asm-id]
               :opt [::source-actor-ident ::event-id ::event-data ::queued-mutations]))

(Defn fake-handler [env] [::env => ::env] env)
;; State Machine Definition Specs
(s/def ::actor-names (s/coll-of ::actor-name :kind set?))
(s/def ::handler (s/with-gen any? #(s/gen #{fake-handler})))
(s/def ::state (s/with-gen (s/keys :req [::handler]) #(s/gen #{{::handler fake-handler}})))
(s/def ::states (s/with-gen (s/map-of keyword? ::state) #(s/gen #{{:initial {::handler fake-handler}}})))
(s/def ::alias keyword?)
(s/def ::aliases (s/map-of ::alias (s/tuple ::actor-name keyword?)))
(s/def ::plugin (s/with-gen any? #(s/gen #{(fn [aliases] nil)})))
(s/def ::plugins (s/map-of keyword? ::plugin))
(s/def ::event-names (s/coll-of keyword? :kind set?))
(s/def ::state-machine-definition (s/with-gen
                                    (s/keys :req [::actor-names ::states] :opt [::aliases ::plugins ::event-names])
                                    #(s/gen #{{::actor-names #{:a}
                                               ::states      {:initial {::handler (fn [env] env)}}}})))

;; ================================================================================
;; State Machine Registry
;; ================================================================================

(def registry (atom {}))

(defn register-state-machine!
  [id definition]
  (swap! registry assoc id definition))

(declare asm-value)

(Defn get-state-machine [id]
  [::state-machine-id => (s/nilable ::state-machine-definition)]
  (get @registry id))

(Defn lookup-state-machine
  [env]
  [::env => (s/nilable ::state-machine-definition)]
  (some->> (asm-value env [::state-machine-id]) (get @registry)))

(Defn lookup-state-machine-field
  [env ks]
  [::env (s/or :k keyword? :kpath vector?) => any?]
  (if (vector? ks)
    (get-in (lookup-state-machine env) ks)
    (get (lookup-state-machine env) ks)))

;; ================================================================================
;; Active State Machine API
;; ================================================================================

(Defn new-asm
  "Create the runtime state for the given state machine in it's initial state.

  - `::state-machine-id` is the globally unique key of for a state machine definition.
  - `::asm-id` is a user-generated unique ID for the instance of the asm. This allows more than one
    instance of the same state machine definition to be active at the same time on the UI.
  - `::actor->ident` is a map from actor name to an ident.

  Returns an active state machine that can be stored in Fulcro state for a specific
  state machine definition."
  [{::keys [state-machine-id asm-id actor->ident]}]
  [(s/keys :req [::state-machine-id ::asm-id ::actor->ident]) => ::asm]
  (let [i->a (set/map-invert actor->ident)]
    {::asm-id           asm-id
     ::state-machine-id state-machine-id
     ::active-state     :initial
     ::ident->actor     i->a
     ::actor->ident     actor->ident
     ::local-storage    {}}))

(Defn asm-path
  "Returns the path to an asm elements in an asm `env`."
  [{::keys [state-map asm-id] :as env} ks]
  [::env (s/or :v vector? :k keyword?) => vector?]
  (let [path (if (vector? ks)
               (into [::state-map ::asm-id asm-id] ks)
               [::state-map ::asm-id asm-id ks])]
    (when (and (log/may-log? :debug) (not (get-in state-map [::asm-id asm-id])))
      (log/warn "Attempt to get an ASM path" ks "for a state machine that is not in Fulcro state. ASM ID: " asm-id))
    path))

(Defn asm-value
  "Get the value of an ASM based on keyword OR key-path `ks`."
  [env ks]
  [::env (s/or :v vector? :k keyword?) => any?]
  (get-in env (asm-path env ks)))

(Defn valid-state?
  [env state-id]
  [::env ::state-id => boolean?]
  (let [states (set/union #{::exit ::started} (-> (lookup-state-machine-field env ::states) keys set))]
    (contains? states state-id)))

(Defn activate
  "Move to the given state. Returns a new env."
  [env state-id]
  [::env ::state-id => ::env]
  (if (valid-state? env state-id)
    (assoc-in env (asm-path env ::active-state) state-id)
    (do
      (log/error "Activate called for invalid state: " state-id)
      env)))

(Defn store
  "Store a k/v pair with the active state machine (will only exist as long as it is active)"
  [env k v]
  [::env keyword? any? => ::env]
  (update-in env (asm-path env ::local-storage) assoc k v))

(Defn retrieve
  "Retrieve the value for a k from the active state machine. See `store`."
  ([env k]
   [::env keyword? => any?]
   (retrieve env k nil))
  ([env k dflt]
   [::env keyword? any? => any?]
   (get-in env (asm-path env [::local-storage k]) dflt)))

(Defn actor->ident
  [env actor-name]
  [::env ::actor-name => (s/nilable ::fulcro-ident)]
  (when-let [lookup (get-in env (asm-path env ::actor->ident))]
    (lookup actor-name)))

(Defn resolve-alias
  "Looks up the given alias in the alias map and returns the real Fulcro state path or nil if no such path exists."
  [env alias]
  [::env ::alias => any?]
  (when-let [resolution-path (lookup-state-machine-field env [::aliases alias])]
    (let [[actor & subpath] resolution-path
          base-path (actor->ident env actor)
          real-path (into base-path subpath)]
      real-path)))

(Defn actor-path
  "Get the real Fulcro state-path for the entity of the given actor."
  ([env actor-name]
   [::env ::actor-name => (s/nilable vector?)]
   (actor-path env actor-name nil))
  ([env actor-name k]
   [::env ::actor-name any? => (s/nilable vector?)]
   (if-let [ident (actor->ident env actor-name)]
     (cond-> ident
       k (conj k))
     nil)))

(Defn set-actor-value
  "Set a value in the actor's Fulcro entity. Only the actor is resolved. The k is not processed as an alias. "
  [env actor-name k v]
  [::env ::actor-name any? any? => ::env]
  (if-let [path (actor-path env actor-name k)]
    (update env ::state-map assoc-in path v)
    env))

(Defn actor-value
  "Get the value of a particular key in the given actor's entity. If follow-idents? is true (which is the default),
  then it will recursively follow idents until it finds a non-ident value."
  ([{::keys [state-map] :as env} actor-name k follow-idents?]
   [::env ::actor-name any? boolean? => any?]
   (when-let [path (actor-path env actor-name k)]
     (loop [v (get-in state-map path) depth 100]
       (if (and follow-idents? (futil/ident? v) (pos-int? depth))
         (recur (get-in state-map v) (dec depth))
         v))))
  ([env actor-name k]
   [::env ::actor-name any? => any?]
   (actor-value env actor-name k true)))

(Defn alias-value
  "Get a Fulcro state value by state machine data alias."
  [{::keys [state-map] :as env} alias]
  [::env keyword? => any?]
  (if-let [real-path (resolve-alias env alias)]
    (get-in state-map real-path)
    (do
      (log/error "Unable to find alias in state machine:" alias)
      nil)))

(Defn set-aliased-value
  [env alias new-value]
  [::env ::alias any? => ::env]
  (if-let [real-path (resolve-alias env alias)]
    (update env ::state-map assoc-in real-path new-value)
    (do
      (log/error "Attempt to set a value on an invalid alias:" alias)
      env)))

(Defn aliased-data
  "Extracts aliased data from Fulcro state to construct arguments. If explicit-args is supplied,
   then that is merged with aliased data, passed to the named plugin.  The return of the plugin is
   the result of this function"
  [env]
  [::env => map?]
  (let [alias-keys (some-> (lookup-state-machine-field env ::aliases) keys)]
    (reduce (fn [result k]
              (assoc result k (alias-value env k)))
      {}
      alias-keys)))

(Defn run
  "Run a state-machine plugin. Extracts aliased data from Fulcro state to construct arguments. If explicit-args is supplied,
   then that is merged with aliased data, passed to the named plugin.  The return of the plugin is
   the result of this function. Plugins cannot side-effect, and are meant for providing external computation algorithms
   that the state machine logic might need. For example, an actor representing a form might need to provide validation
   logic.

   If explicit-args are passed, then they will take *precedence* over the auto-extracted aliased data that is passed to
   the plugin."
  ([env plugin-name]
   [::env keyword? => any?]
   (run env plugin-name nil))
  ([env plugin-name explicit-args]
   [::env keyword? (s/nilable map?) => any?]
   (when-let [plugin (lookup-state-machine-field env [::plugins plugin-name])]
     (let [params (merge (aliased-data env) explicit-args)]
       (plugin params)))))

(Defn exit
  "Indicate that the state machine is done."
  [env]
  [::env => ::env]
  (activate env ::exit))

(Defn apply-event-value
  [env {::keys [event-id event-data]}]
  [::env (s/keys :opt [::event-id ::event-data]) => ::env]
  (let [alias (::alias event-data)
        value (:value event-data)]
    (cond-> env
      (and (= ::value-changed event-id) alias)
      (set-aliased-value alias value))))

(Defn state-machine-env [state-map ref asm-id event-id event-data]
  [::state-map (s/nilable ::fulcro-ident) ::asm-id (s/nilable ::event-id) (s/nilable ::event-data) => ::env]
  (cond-> {::state-map state-map
           ::asm-id    asm-id}
    event-id (assoc ::event-id event-id)
    (seq event-data) (assoc ::event-data event-data)
    ref (assoc ::source-actor-ident ref)))

(defn- defer [f]
  #?(:cljs (js/setTimeout f 0)
     :clj  (f)))

(defn trigger-state-machine-event!
  "Low-level implementation of triggering a state machine event. Does no direct interaction with
  Fulcro UI refresh.  Use `trigger!`.

  - `env` - A fulcro mutation env, containing at least the state atom and optionally the ref of the
    component that was the source of the event.
  - params - The parameters for the event

  Returns a vector of actor idents that should be refreshed. "
  [{:keys [reconciler component state ref]} {::keys [event-id event-data asm-id] :as params}]
  (let [sm-env           (state-machine-env @state ref asm-id event-id event-data)
        smdef            (lookup-state-machine sm-env)
        actor-idents     (mapv #(actor->ident sm-env %) (::actor-names smdef))
        current-state    (asm-value sm-env ::active-state)
        handler          (get-in smdef [::states current-state ::handler] identity)
        final-env        (-> sm-env
                           (apply-event-value params)
                           (handler))
        queued-mutations (::queued-mutations final-env)
        queued-loads     (::queued-loads final-env)
        next-state       (when final-env (asm-value final-env ::active-state))]
    (when (seq queued-mutations)
      (doseq [{::keys [mutation-context] :as mutation-params} queued-mutations]
        (let [contextual-component (prim/ref->any reconciler (actor->ident sm-env mutation-context))]
          (if (nil? contextual-component)
            (log/error "Cannot run mutation. No contextual component mounted for " mutation-context)
            (pm/pmutate! contextual-component mutation-delegate mutation-params)))))
    (when (seq queued-loads)
      (log/info "Queued Loads " queued-loads)
      (doseq [{::prim/keys [component-class]
               ::keys      [actor-name query-key load-options] :as load-params} queued-loads]
        (if actor-name                                      ; actor-centric load
          (let [actor-ident (actor->ident sm-env actor-name)
                cls         (or component-class (prim/react-type (prim/ref->any reconciler actor-ident)))]
            (if (nil? cls)
              (log/error "Cannot run load. Counld not derive class (and none was passed) for " actor-name)
              (defer #(df/load reconciler actor-ident cls load-options))))
          (if (and (nil? query-key) (nil? component-class)) ; regular-style load
            (log/error "Cannot run load. Either query-key or component-class is required.")
            (defer #(df/load reconciler query-key component-class load-options))))))
    (when-let [new-fulcro-state (some-> (::state-map final-env)
                                  ;; GC state machine if it exited
                                  (cond->
                                    (= ::exit next-state) (update ::asm-id dissoc asm-id)))]
      (reset! state new-fulcro-state))
    actor-idents))

(defmutation trigger-state-machine-event
  "Mutation: Trigger an event on an active state machine"
  [{::keys [event-id event-data asm-id] :as params}]
  (action [{:keys [reconciler] :as env}]
    (log/debug "Triggering state machine event " event-id)
    (let [to-refresh (trigger-state-machine-event! env params)]
      (log/debug "Queuing actor refreshes" to-refresh)
      (fcip/queue! reconciler to-refresh))
    true))

(mi/declare-mutation trigger-state-machine-event `trigger-state-machine-event)

(defn trigger!
  "Trigger an event on an active state machine. Safe to use in mutation bodies."
  ([this active-state-machine-id event-id] (trigger! this active-state-machine-id event-id {}))
  ([this active-state-machine-id event-id extra-data]
    #?(:cljs
       (js/setTimeout
         #(prim/transact! this [(trigger-state-machine-event {::asm-id     active-state-machine-id
                                                              ::event-id   event-id
                                                              ::event-data extra-data})])
         0))))

(defn set-string!
  "Similar to Fulcro's set-string, but it sets the string on an active state machine's data alias.
  event-or-string can be a string or a React DOM onChange event."
  [this active-state-machine-id alias event-or-string]
  (let [value (if (string? event-or-string)
                event-or-string
                (or (some-> event-or-string .-target .-value) ""))]
    (trigger! this active-state-machine-id ::value-changed {::alias alias
                                                            :value  value})))

(defmutation begin
  "Mutation to begin a state machine. Use `begin!` instead."
  [{::keys [asm-id] :as params}]
  (action [{:keys [state] :as env}]
    (swap! state (fn [s]
                   (-> s
                     (assoc-in [::asm-id asm-id] (new-asm params)))))
    (trigger-state-machine-event! env {::event-id   ::started
                                       ::asm-id     asm-id
                                       ::event-data {}})))
(mi/declare-mutation begin `begin)

(Defn begin!
  "Install and start a state machine.

  this - A UI component or reconciler
  machine - A state machine defined with defstatemachine
  instance-id - An ID by which you will refer to this active instance.
  actors - A map of actor-names -> The idents of the real components that will represent them in the UI."
  [this machine instance-id actors]
  [(s/or :c ::prim/component :r ::fulcro-reconciler) ::state-machine-definition ::asm-id ::actor->ident => any?]
  (prim/transact! this [(begin {::asm-id instance-id ::state-machine-id (::state-machine-id machine) ::actor->ident actors})]))

#?(:clj
   (defmacro defstatemachine [name body]
     (let [nmspc       (str (ns-name *ns*))
           storage-sym (symbol nmspc (str name))]
       `(do
          (def ~name (assoc ~body ::state-machine-id '~storage-sym))
          (register-state-machine! '~storage-sym ~body)))))

;; ================================================================================
;; I/O Integration: remote mutations
;; ================================================================================

(s/def ::target-actor ::actor-name)
(s/def ::target-alias ::alias)
(s/def ::ok-event ::event-id)
(s/def ::error-event ::event-id)
(s/def ::ok-data map?)
(s/def ::error-data map?)
(s/def ::mutation (s/with-gen symbol? #(s/gen #{`do-something})))
(mi/declare-mutation spec-mutation `spec-mutation)
(s/def ::mutation-decl (s/with-gen mi/mutation-declaration? #(s/gen #{spec-mutation})))
(s/def ::mutation-context ::actor-name)
(s/def ::mutation-descriptor (s/keys :req [::mutation-context]
                               :opt [::pm/target ::ok-event ::ok-data ::error-event ::error-data
                                     ::pm/returning ::mutation ::mutation-remote]))
(s/def ::mutation-remote keyword?)
(s/def ::queued-mutations (s/coll-of ::mutation-descriptor))

(Defn compute-target
  "Compute a raw Fulcro target based on the possible options.

  `env` - The SM env

  targeting options:

  `::pm/target explicit-target` - A raw Fulcro data fetch target.
  `::uism/target-actor actor-alias` - Helper that can translate an actor alias to a target
  `::uism/target-alias field-alias` - Helper that can translate a data alias to a target (ident + field)

  If more than one option is used, then `df/mutliple-targets` will be used to encode them all.
  "
  [env {::pm/keys [target]
        ::keys    [target-actor target-alias]}]
  [::env (s/keys :opt [::pm/target ::target-actor ::target-alias]) => (s/nilable vector?)]
  (let [noptions (count (keep identity [target target-actor target-alias]))
        actor    (when target-actor (actor->ident env target-actor))
        field    (when target-alias (resolve-alias env target-alias))]
    (if (> noptions 1)
      (if (and target (dft/multiple-targets? target))
        (into target (keep identity [actor field]))
        (apply df/multiple-targets (keep identity [target actor field])))
      (or target actor field))))

(let [mtrigger! (fn mutation-trigger* [{:keys [reconciler state]} actor-ident asm-id event data]
                  (let [response   (get-in @state (conj actor-ident ::pm/mutation-response))
                        event-data (merge {} data response)]
                    #?(:cljs
                       (js/setTimeout
                         #(prim/transact! reconciler actor-ident [(trigger-state-machine-event {::asm-id     asm-id
                                                                                                ::event-id   event
                                                                                                ::event-data event-data})])
                         0))))]
  (defmethod m/mutate `mutation-delegate [{:keys [reconciler state] :as env} _
                                          {::keys [asm-id ok-event error-event mutation
                                                   mutation-context ok-data error-data mutation-remote] :as mp}]
    ;; mutation can be run for figuring out remote
    (if (or (empty? @state) (empty? mp))
      {(or mutation-remote :remote) (pm/pessimistic-mutation env)}
      (let [sm-env      (state-machine-env @state nil asm-id ok-event ok-data)
            actor-ident (actor->ident sm-env mutation-context)
            fixed-env   (-> env
                          (assoc :ref actor-ident)
                          (update :ast assoc
                            :key mutation
                            :dispatch-key mutation
                            :params (dissoc mp ::ok-event ::error-event ::mutation
                                      ::mutation-context ::ok-data ::error-data
                                      ::mutation-remote ::asm-id)))]
        (cond-> {(or mutation-remote :remote) (pm/pessimistic-mutation fixed-env)}
          ok-event (assoc :ok-action (fn [] (mtrigger! fixed-env actor-ident asm-id ok-event ok-data)))
          error-event (assoc :error-action (fn [] (mtrigger! fixed-env actor-ident asm-id error-event error-data))))))))

(Defn trigger-remote-mutation
  "Run the given REMOTE mutation (a symbol or mutation declaration) in the context of the state machine.

  `env` - The SM handler environment
  `actor` - The name (keyword) of a defined actor.  The mutation will be run in the context of this actor's state
            (see pm/pmutate!), which means that progress will be visible there. THERE MUST BE A MOUNTED COMPONENT
            with this actor's name ON the UI, or the mutation will abort.
  `mutation` - The symbol (or mutation declaration) of the *server* mutation to run. This function will *not* run a local
  version of the mutation.
  `options-and-params` - The parameters to pass to your mutation. This map can also include these additional
  state-machine options:

  `::pm/returning Class` - Option of pmutate to supply a component form normalizing the returned result.
  `::pm/target explicit-target` - Option of pmutate for targeting retuned result.
  `::uism/target-actor actor` - Helper that can translate an actor name to a target, if returning a result.
  `::uism/target-alias field-alias` - Helper that can translate a data alias to a target (ident + field).
  `::uism/ok-event event-id` - The SM event to trigger when the pessimistic mutation succeeds (no default).
  `::uism/error-event event-id` - The SM event to trigger when the pessimistic mutation fails (no default).
  `::uism/ok-data map-of-data` - Data to include in the event-data on an ok event
  `::uism/error-data map-of-data` - Data to include in the event-data on an error event
  `::uism/mutation-remote` - The keyword name of the Fulcro remote (defaults to :remote)

  NOTE: The mutation response *will be merged* into the event data that is sent to the SM handler.

  This function does *not* side effect.  It queues the mutation to run after the handler exits."
  [env actor mutation options-and-params]
  [::env
   ::actor-name
   (s/or :sym ::mutation :decl ::mutation-decl)
   (s/keys :opt [::pm/returning ::pm/target ::target-actor
                 ::target-alias ::ok-event ::error-event
                 ::ok-data ::error-data ::mutation-remote])
   => ::env]
  (let [target              (compute-target env options-and-params)
        asm-id              (::asm-id env)
        mutation-sym        (mi/mutation-symbol mutation {})
        mutation-descriptor (-> options-and-params
                              (dissoc ::target-actor ::target-alias ::pm/target)
                              (assoc ::asm-id asm-id ::mutation mutation-sym ::mutation-context actor)
                              (cond->
                                (seq target) (assoc ::pm/target target)))]
    (update env ::queued-mutations (fnil conj []) mutation-descriptor)))

;; ================================================================================
;; I/O: Load integration
;; ================================================================================

(s/def ::load-options map?)
(s/def ::load (s/keys :opt [::query-key ::prim/component-class ::load-options]))
(s/def ::queued-loads (s/coll-of ::load))

(Defn load
  "Identical API to fulcro's data fetch `load`, but using a handle `env` instead of a component/reconciler.
   Adds the load request to then env which will be sent to Fulcro as soon as the handler finishes."
  ([env k component-class]
   [::env keyword? ::prim/component-class => ::env]
   (load env k component-class {}))
  ([env k component-class options]
   [::env keyword? ::prim/component-class ::load-options => ::env]
   (let [{::keys [post-event post-event-params]} options
         {::keys [asm-id]} env
         options (cond-> options
                   post-event (assoc :post-mutation `trigger-state-machine-event
                                     :post-mutation-params (merge post-event-params
                                                             {::asm-id     asm-id
                                                              ::event-id   post-event
                                                              ::event-data post-event-params}))
                   :and (dissoc ::post-event ::post-event-params))]
     (update env ::queued-loads (fnil conj []) (cond-> {}
                                                 component-class (assoc ::prim/component-class component-class)
                                                 k (assoc ::query-key k)
                                                 options (assoc ::load-options options))))))

(Defn load-actor
  "Load (refresh) the given actor. If the actor *is not* on the UI, then you *must* specify
   `:fulcro.client.primitives/component-class` in the `options` map.

   options can contain the normal `df/load` parameters, and also:

   `::prim/component-class` - The defsc name of the component to use for normalization and query. Only needed if the
     actor is not yet on-screen.

   Adds a load request to then env which will be sent to Fulcro as soon as the handler finishes."
  ([env actor-name]
   [::env ::actor-name => ::env]
   (load-actor env actor-name {}))
  ([env actor-name {::prim/keys [component-class] :as options}]
   [::env ::actor-name ::load-options => ::env]
   (update env ::queued-loads (fnil conj []) (cond-> {::actor-name   actor-name
                                                      ::load-options (dissoc options ::prim/component-class)}
                                               component-class (assoc ::prim/component-class component-class)))))
