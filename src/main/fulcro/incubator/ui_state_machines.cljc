(ns fulcro.incubator.ui-state-machines
  #?(:cljs (:require-macros [fulcro.incubator.ui-state-machines]))
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [fulcro.client.mutations :as m :refer [defmutation]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.client.impl.protocols :as fcip]
    [fulcro.incubator.mutation-interface :as mi]
    [fulcro.incubator.spec-helpers :refer [Defn =>]]
    [fulcro.util :as futil]
    [taoensso.timbre :as log]))

;; Active State Machine and ENV specs
(s/def ::state-map map?)
(s/def ::fulcro-ident (s/with-gen futil/ident? #(s/gen #{[:table 1] [:other :tab]})))
(s/def ::fulcro-reconciler (s/with-gen prim/reconciler? #(s/gen #{(prim/reconciler {})})))
(s/def ::fulcro-component (s/with-gen prim/component? #(s/gen #{#js {:fulcro$isComponent true}})))
(s/def ::source-actor-ident ::fulcro-ident)
(s/def ::actor-name keyword?)
(s/def ::actor->ident (s/map-of ::actor-name ::fulcro-ident))
(s/def ::ident->actor (s/map-of ::fulcro-ident ::actor-name))
(s/def ::active-state keyword?)                             ; The state the active instance is currently in
(s/def ::state-machine-id symbol?)                          ; The symbol of the state machine's definition
(s/def ::asm-id any?)                                       ; The ID of the active instance in fulcro state
(s/def ::local-storage (s/map-of keyword? any?))
;; aliases and plugins are copied from the definition
(s/def ::asm (s/keys :req [::asm-id ::state-machine-id ::active-state ::actor->ident ::ident->actor]
               :opt [::local-storage]))
(s/def ::state-id keyword?)
(s/def ::event-data map?)
(s/def ::event-id keyword?)
(s/def ::env (s/keys :req [::state-map ::asm-id]
               :opt [::source-actor-ident ::event-id ::event-data]))

;; State Machine Definition Specs
(s/def ::actor-names (s/coll-of ::actor-name :kind set?))
(s/def ::handler (s/fspec :args (s/cat :env ::env) :ret ::env))
(s/def ::state (s/keys :req [::handler]))
(s/def ::states (s/map-of keyword? ::state))
(s/def ::aliases (s/map-of keyword? (s/tuple ::actor-name keyword?)))
(s/def ::plugin (s/fspec :args (s/cat :aliased-data map?) :ret any?))
(s/def ::plugins (s/map-of keyword? ::plugin))
(s/def ::event-names (s/coll-of keyword? :kind set?))
(s/def ::state-machine-definition (s/keys :req [::actor-names ::states] :opt [::aliases ::plugins ::event-names]))

;; ================================================================================
;; State Machine Registry
;; ================================================================================

(def registry (atom {}))

(defn register-state-machine!
  [id definition]
  (swap! registry assoc id definition))

(declare asm-value)

(Defn get-state-machine [id]
  [::state-machine-id => ::state-machine-definition]
  (get @registry id))

(Defn lookup-state-machine
  [env]
  [::env => ::state-machine-definition]
  (some->> (asm-value env [::state-machine-id]) (get @registry)))

(Defn lookup-state-machine-field
  [env ks]
  [::env (s/or :k keyword? :kpath vector?) => ::state-machine-definition]
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
  [::env (s/or :v vector? :k keyword?) => vector?]
  (get-in env (asm-path env ks)))

(Defn valid-state?
  [env state-id]
  [::env ::state-id => boolean?]
  (let [states (-> (lookup-state-machine-field env ::states) keys set)]
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
  "Looks up the given k in the alias map and returns the real Fulcro state path or nil if no such path exists."
  [env alias]
  [::env any? => any?]
  (when-let [resolution-path (lookup-state-machine-field env [::aliases alias])]
    (let [[actor & subpath] resolution-path
          base-path (actor->ident env actor)
          real-path (into base-path subpath)]
      real-path)))

(Defn actor-path
  "Get the state-path for the entity of the given actor."
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
  [::env keyword? any? => ::env]
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

(defn trigger-state-machine-event!
  "Low-level implementation of triggering a state machine event. Does no direct interaction with
  Fulcro UI refresh.  Use `trigger!`.

  - `env` - A fulcro mutation env, containing at least the state atom and optionally the ref of the
    component that was the source of the event.
  - params - The parameters for the event

  Returns a vector of actor idents that should be refreshed. "
  [{:keys [state ref]} {::keys [event-id event-data asm-id] :as params}]
  (let [sm-env        (cond-> {::state-map @state
                               ::asm-id    asm-id}
                        event-id (assoc ::event-id event-id)
                        (seq event-data) (assoc ::event-data event-data)
                        ref (assoc ::source-actor-ident ref))
        smdef         (lookup-state-machine sm-env)
        actor-idents  (mapv #(actor->ident sm-env %) (::actor-names smdef))
        current-state (asm-value sm-env ::active-state)
        handler       (get-in smdef [::states current-state ::handler] identity)
        final-env     (-> sm-env
                        (apply-event-value params)
                        (handler))
        next-state    (when final-env (asm-value final-env ::active-state))]
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
  [(s/or :c ::fulcro-component :r ::fulcro-reconciler) ::state-machine-definition ::asm-id ::actor->ident => any?]
  (prim/transact! this [(begin {::asm-id instance-id ::state-machine-id (::state-machine-id machine) ::actor->ident actors})]))

#?(:clj
   (defmacro defstatemachine [name body]
     (let [nmspc       (str (ns-name *ns*))
           storage-sym (symbol nmspc (str name))]
       `(do
          (def ~name (assoc ~body ::state-machine-id '~storage-sym))
          (register-state-machine! '~storage-sym ~body)))))
