(ns fulcro.incubator.ui-state-machines
  #?(:cljs (:require-macros fulcro.incubator.ui-state-machines))
  (:refer-clojure :exclude [load])
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [ghostwheel.core :as gw :refer [>fdef => | ? <-]]
    [fulcro.logging :as log]
    [fulcro.client.data-fetch :as df]
    [fulcro.client.util]
    [fulcro.client.impl.data-targeting :as dft]
    [fulcro.client.mutations :as m :refer [defmutation]]
    [fulcro.client.primitives :as prim :refer [defsc]]
    [fulcro.incubator.mutation-interface :as mi]
    [fulcro.incubator.pessimistic-mutations :as pm]
    [fulcro.util :as futil]))

(mi/declare-mutation mutation-delegate `mutation-delegate)

(defn defer
  "No effect in CLJ. In CLJS, defer calling `f` until the current UI thread is released (setTimeout 0)."
  [f]
  #?(:cljs (js/setTimeout f 0)
     :clj  (f)))

(defn set-js-timeout! [f tm]
  #?(:clj  (f)
     :cljs (js/setTimeout f tm)))

(defn clear-js-timeout! [timer]
  #?(:cljs (js/clearTimeout timer)))

(defn- is-atom?
  "Returns TRUE when x is an atom."
  [x]
  #?(:cljs (instance? cljs.core.Atom x)
     :clj  (instance? clojure.lang.Atom x)))

(s/def ::atom (s/with-gen is-atom? #(s/gen #{(atom {}) (atom #{}) (atom nil)})))

;; Active State Machine and ENV specs
(s/def ::state-map map?)
(s/def ::fulcro-ident (s/with-gen futil/ident? #(s/gen #{[:table 1] [:other :tab]})))
(s/def ::refresh-vector (s/with-gen (s/coll-of ::fulcro-ident :kind vector?) #(s/gen [[:table 1] [:other :tab]])))
(s/def ::fulcro-reconciler (s/with-gen prim/reconciler? #(s/gen #{(prim/reconciler {})})))
(s/def ::source-actor-ident ::fulcro-ident)
(s/def ::actor-name keyword?)
(s/def ::actor->component-name (s/map-of ::actor-name keyword?))
(s/def ::actor->ident (s/map-of ::actor-name ::fulcro-ident))
(s/def ::ident->actor (s/map-of ::fulcro-ident ::actor-name))
(s/def ::active-state keyword?)                             ; The state the active instance is currently in
(s/def ::state-machine-id (s/with-gen symbol? #(s/gen #{'the-state-machine}))) ; The symbol of the state machine's definition
(s/def ::asm-id any?)                                       ; The ID of the active instance in fulcro state
(s/def ::local-storage (s/map-of keyword? any?))
(s/def ::timeout pos-int?)
(s/def ::timer-id (s/with-gen any? #(s/gen #{:timer-1 42})))
(s/def ::cancel-fn (s/with-gen (s/or :f fn? :s set?) #(s/gen #{#{:event! :other!}})))
(s/def ::cancel-on (s/with-gen (fn fn-or-set* [i] (let [f (-> i meta :cancel-on)]
                                                    (or (fn? f) (set? f)))) #(s/gen #{(with-meta {} {:cancel-on (fn [e] true)})})))
(s/def ::js-timer (s/with-gen #(-> % meta :timer boolean) #(s/gen #{(with-meta {} {:timer {}})})))
(s/def ::timeout-descriptor (s/keys :req [::js-timer ::timeout ::event-id ::timer-id ::cancel-on] :opt [::event-data]))
(s/def ::queued-timeouts (s/coll-of ::timeout-descriptor))
(s/def ::active-timers (s/map-of ::timer-id ::timeout-descriptor))
(s/def ::asm (s/keys :req [::asm-id ::state-machine-id ::active-state ::actor->ident ::actor->component-name
                           ::ident->actor ::active-timers ::local-storage]))
(s/def ::state-id keyword?)
(s/def ::event-data map?)
(s/def ::event-id keyword?)
(s/def ::trigger-descriptor (s/keys :req [::asm-id ::event-id] :opt [::event-data]))
(s/def ::queued-triggers (s/coll-of ::trigger-descriptor))
(s/def ::env (s/keys :req [::state-map ::asm-id]
               :opt [::source-actor-ident ::event-id ::event-data ::queued-triggers
                     ::queued-mutations ::queued-loads ::queued-timeouts]))

(defn fake-handler [env] [::env => ::env] env)
(>fdef fake-handler [env] [::env => ::env])

;; State Machine Definition Specs
(s/def ::actor-names (s/coll-of ::actor-name :kind set?))
(s/def ::event-predicate (s/with-gen fn? #(s/gen #{(fn [_] false) (fn [_] true)})))
(s/def ::handler (s/with-gen fn? #(s/gen #{fake-handler})))
(s/def ::target-state ::state-id)
(s/def ::event-processing (s/keys :opt [::handler ::event-predicate ::target-state]))
(s/def ::events (s/map-of ::event-id ::event-processing))
(s/def ::state (s/with-gen
                 (s/or
                   :handler (s/keys :req [::handler])
                   :events (s/keys :req [::events]))
                 #(s/gen #{{::handler fake-handler}})))
(s/def ::states (s/with-gen (s/map-of ::state-id ::state) #(s/gen #{{:initial {::handler fake-handler}}})))
(s/def ::alias keyword?)
(s/def ::aliases (s/map-of ::alias (s/coll-of (s/or :index int? :key keyword?) :kind vector?)))
(s/def ::plugin (s/with-gen any? #(s/gen #{(fn [aliases] nil)})))
(s/def ::plugins (s/map-of keyword? ::plugin))
(s/def ::event-names (s/coll-of keyword? :kind set?))
(s/def ::target-state keyword?)
(s/def ::state-machine-definition (s/with-gen
                                    (s/keys :req [::states] :opt [::actor-names ::aliases ::plugins ::event-names])
                                    #(s/gen #{{::actor-names #{:a}
                                               ::states      {:initial {::handler (fn [env] env)}}}})))

;; ================================================================================
;; State Machine Registry
;; ================================================================================

(def registry (atom {}))
(defn register-state-machine! [id definition] (swap! registry assoc id definition))
(declare asm-value)

(defn get-state-machine [id] (get @registry id))
(>fdef get-state-machine [id] [::state-machine-id => (s/nilable ::state-machine-definition)])

(defn lookup-state-machine [env] (some->> (asm-value env [::state-machine-id]) (get @registry)))
(>fdef lookup-state-machine [env] [::env => (s/nilable ::state-machine-definition)])

(defn lookup-state-machine-field
  [env ks]
  [::env (s/or :k keyword? :kpath vector?) => any?]
  (if (vector? ks)
    (get-in (lookup-state-machine env) ks)
    (get (lookup-state-machine env) ks)))
(>fdef lookup-state-machine-field
  [env ks]
  [::env (s/or :k keyword? :kpath vector?) => any?])

;; ================================================================================
;; Active State Machine API
;; ================================================================================

(mi/declare-mutation trigger-state-machine-event `trigger-state-machine-event)

(defn trigger!
  "Trigger an event on an active state machine. Safe to use in mutation bodies."
  ([this active-state-machine-id event-id] (trigger! this active-state-machine-id event-id {}))
  ([this active-state-machine-id event-id extra-data]
   (log/debug "Triggering" event-id "on" active-state-machine-id "with" extra-data)
   (defer
     #(prim/transact! this [(trigger-state-machine-event {::asm-id     active-state-machine-id
                                                          ::event-id   event-id
                                                          ::event-data extra-data})]))))


(defn new-asm
  "Create the runtime state for the given state machine in it's initial state.

  - `::state-machine-id` is the globally unique key of for a state machine definition.
  - `::asm-id` is a user-generated unique ID for the instance of the asm. This allows more than one
    instance of the same state machine definition to be active at the same time on the UI.
  - `::actor->ident` is a map from actor name to an ident.

  Returns an active state machine that can be stored in Fulcro state for a specific
  state machine definition."
  [{::keys [state-machine-id asm-id actor->ident actor->component-name]}]
  (let [i->a (set/map-invert actor->ident)]
    {::asm-id                asm-id
     ::state-machine-id      state-machine-id
     ::active-state          :initial
     ::ident->actor          i->a
     ::actor->ident          actor->ident
     ::actor->component-name (or actor->component-name {})
     ::active-timers         {}
     ::local-storage         {}}))
(>fdef new-asm [options]
  [(s/keys :req [::state-machine-id ::asm-id ::actor->ident]) => ::asm])

(defn asm-path
  "Returns the path to an asm elements in an asm `env`."
  [{::keys [state-map asm-id] :as env} ks]
  (let [path (if (vector? ks)
               (into [::state-map ::asm-id asm-id] ks)
               [::state-map ::asm-id asm-id ks])]
    (when (not (get-in state-map [::asm-id asm-id]))
      (log/debug "Attempt to get an ASM path" ks "for a state machine that is not in Fulcro state. ASM ID: " asm-id))
    path))
(>fdef asm-path [env ks] [::env (s/or :v vector? :k keyword?) => vector?])

(defn asm-value
  "Get the value of an ASM based on keyword OR key-path `ks`."
  [env ks]
  (get-in env (asm-path env ks)))
(>fdef asm-value [env ks] [::env (s/or :v vector? :k keyword?) => any?])

(defn valid-state?
  [env state-id]
  (let [states (set/union #{::exit ::started} (-> (lookup-state-machine-field env ::states) keys set))]
    (contains? states state-id)))
(>fdef valid-state? [env state-id] [::env ::state-id => boolean?])

(defn activate
  "Move to the given state. Returns a new env."
  [env state-id]
  (if (valid-state? env state-id)
    (do
      (log/debug "Activating state " state-id "on" (::asm-id env))
      (assoc-in env (asm-path env ::active-state) state-id))
    (do
      (log/error "Activate called for invalid state: " state-id "on" (::asm-id env))
      env)))
(>fdef activate [env state-id] [::env ::state-id => ::env])

(defn store
  "Store a k/v pair with the active state machine (will only exist as long as it is active)"
  [env k v]
  (log/debug "Storing" k "->" v "on" (::asm-id env))
  (update-in env (asm-path env ::local-storage) assoc k v))
(>fdef store [env k v] [::env keyword? any? => ::env])

(defn retrieve
  "Retrieve the value for a k from the active state machine. See `store`."
  ([env k]
   (retrieve env k nil))
  ([env k dflt]
   (get-in env (asm-path env [::local-storage k]) dflt)))
(>fdef retrieve
  ([env k] [::env keyword? => any?])
  ([env k dflt] [::env keyword? any? => any?]))

(defn actor->ident
  [env actor-name]
  (when-let [lookup (get-in env (asm-path env ::actor->ident))]
    (lookup actor-name)))
(>fdef actor->ident [env actor-name] [::env ::actor-name => (s/nilable ::fulcro-ident)])

(defn resolve-alias
  "Looks up the given alias in the alias map and returns the real Fulcro state path or nil if no such path exists."
  [env alias]
  (when-let [resolution-path (lookup-state-machine-field env [::aliases alias])]
    (let [[actor & subpath] resolution-path
          base-path (actor->ident env actor)
          real-path (into base-path subpath)]
      real-path)))
(>fdef resolve-alias [env alias] [::env ::alias => any?])

(defn actor-path
  "Get the real Fulcro state-path for the entity of the given actor."
  ([env actor-name]
   (actor-path env actor-name nil))
  ([env actor-name k]
   (if-let [ident (actor->ident env actor-name)]
     (cond-> ident
       k (conj k))
     nil)))
(>fdef actor-path
  ([env actor-name] [::env ::actor-name => (s/nilable vector?)])
  ([env actor-name k] [::env ::actor-name any? => (s/nilable vector?)]))

(defn set-actor-value
  "Set a value in the actor's Fulcro entity. Only the actor is resolved. The k is not processed as an alias. "
  [env actor-name k v]
  (if-let [path (actor-path env actor-name k)]
    (update env ::state-map assoc-in path v)
    env))
(>fdef set-actor-value [env actor-name k v] [::env ::actor-name any? any? => ::env])

(defn actor-value
  "Get the value of a particular key in the given actor's entity. If follow-idents? is true (which is the default),
  then it will recursively follow idents until it finds a non-ident value."
  ([{::keys [state-map] :as env} actor-name k follow-idents?]
   (when-let [path (actor-path env actor-name k)]
     (loop [v (get-in state-map path) depth 100]
       (if (and follow-idents? (futil/ident? v) (pos-int? depth))
         (recur (get-in state-map v) (dec depth))
         v))))
  ([env actor-name k]
   (actor-value env actor-name k true)))
(>fdef actor-value
  ([env actor-name k follow-idents?] [::env ::actor-name any? boolean? => any?])
  ([env actor-name k] [::env ::actor-name any? => any?]))

(defn alias-value
  "Get a Fulcro state value by state machine data alias."
  [{::keys [state-map] :as env} alias]
  (if-let [real-path (resolve-alias env alias)]
    (get-in state-map real-path)
    (do
      (log/error "Unable to find alias in state machine:" alias)
      nil)))
(>fdef alias-value [env alias] [::env keyword? => any?])

(defn set-aliased-value
  ([env alias new-value alias-2 value-2 & kv-pairs]
   (let [kvs (into [[alias new-value] [alias-2 value-2]] (partition 2 kv-pairs))]
     (reduce
       (fn [e [k v]]
         (set-aliased-value e k v))
       env
       kvs)))
  ([env alias new-value]
   (if-let [real-path (resolve-alias env alias)]
     (do
       (log/debug "Updating value for " (::asm-id env) "alias" alias "->" new-value)
       (update env ::state-map assoc-in real-path new-value))
     (do
       (log/error "Attempt to set a value on an invalid alias:" alias)
       env))))
(>fdef set-aliased-value
  ([env alias new-value alias-2 value-2 & kv-pairs] [::env ::alias any? ::alias any? (s/* any?) => ::env])
  ([env alias new-value] [::env ::alias any? => ::env]))

(defn aliased-data
  "Extracts aliased data from Fulcro state to construct arguments. If explicit-args is supplied,
   then that is merged with aliased data, passed to the named plugin.  The return of the plugin is
   the result of this function"
  [env]
  (let [alias-keys (some-> (lookup-state-machine-field env ::aliases) keys)]
    (reduce (fn [result k]
              (assoc result k (alias-value env k)))
      {}
      alias-keys)))
(>fdef aliased-data [env] [::env => map?])

(defn run
  "Run a state-machine plugin. Extracts aliased data from Fulcro state to construct arguments. If explicit-args is supplied,
   then that is merged with aliased data, passed to the named plugin.  The return of the plugin is
   the result of this function. Plugins cannot side-effect, and are meant for providing external computation algorithms
   that the state machine logic might need. For example, an actor representing a form might need to provide validation
   logic.

   If explicit-args are passed, then they will take *precedence* over the auto-extracted aliased data that is passed to
   the plugin."
  ([env plugin-name]
   (run env plugin-name nil))
  ([env plugin-name explicit-args]
   (when-let [plugin (lookup-state-machine-field env [::plugins plugin-name])]
     (let [params (merge (aliased-data env) explicit-args)]
       (plugin params)))))
(>fdef run
  ([env plugin-name] [::env keyword? => any?])
  ([env plugin-name explicit-args] [::env keyword? (s/nilable map?) => any?]))

(defn exit
  "Indicate that the state machine is done."
  [env]
  (log/debug "Exiting state machine" (::asm-id env))
  (activate env ::exit))
(>fdef exit [env] [::env => ::env])

(defn apply-event-value
  [env {::keys [event-id event-data]}]
  (let [alias (::alias event-data)
        value (:value event-data)]
    (cond-> env
      (and (= ::value-changed event-id) alias)
      (set-aliased-value alias value))))
(>fdef apply-event-value [env {::keys [event-id event-data]}]
  [::env (s/keys :opt [::event-id ::event-data]) => ::env])

(defn state-machine-env
  "Create an env for use with other functions. Used internally, but may be used as a helper ."
  ([state-map asm-id]
   (state-machine-env state-map nil asm-id nil nil))
  ([state-map ref asm-id event-id event-data]
   (cond-> {::state-map state-map
            ::asm-id    asm-id}
     event-id (assoc ::event-id event-id)
     (seq event-data) (assoc ::event-data event-data)
     ref (assoc ::source-actor-ident ref))))
(>fdef state-machine-env
  ([state-map asm-id]
   [::state-map ::asm-id => ::env])
  ([state-map ref asm-id event-id event-data]
   [::state-map (s/nilable ::fulcro-ident) ::asm-id (s/nilable ::event-id) (s/nilable ::event-data) => ::env]))

(defn with-actor-class
  "Associate a given component UI Fulcro class with an ident.  This is used with `begin!` in your actor map if the
  actor in question is going to be used with loads or mutations that return a value of that type. The actor's class
  can be retrieved for use in a handler using `(uism/actor-class env)`.

  ```
  (begin! ... {:person (uism/with-actor-class [:person/by-id 1] Person)})
  ```
  "
  [ident class]
  (vary-meta ident assoc ::class class))
(>fdef with-actor-class [ident class]
  [::fulcro-ident ::prim/component-class => ::fulcro-ident])

(defn any->actor-component-registry-key
  "Convert one of the possible inputs for an actor into an actor component registry key.

  v can be an ident with actor metadata (see `with-actor-class`), a Fulcro runtime instance whose `get-ident` returns
  a valid ident, or a Fulcro component class with a singleton ident.

  Returns the Fulcro component registry key (a keyword) that will be able to find the real Fulcro
  component for `v`."
  [v]
  (when-let [cls (cond
                   (and (futil/ident? v) (prim/component-class? (some-> v meta ::class))) (some-> v meta ::class)
                   (and (prim/component? v) (-> (prim/get-ident v) second)) (prim/react-type v)
                   (and (prim/component-class? v) (-> (prim/get-ident v {}) second)) v
                   :otherwise nil)]
    (let [str-name (prim/component-name cls)
          [ns nm] (str/split str-name #"/")
          k        (keyword ns nm)]
      k)))
(>fdef any->actor-component-registry-key [v]
  [any? => (s/nilable keyword?)])

(defn actor-class
  "Returns the Fulcro component class that for the given actor, if set."
  [env actor-name]
  (let [actor->component-name (asm-value env ::actor->component-name)
        cls                   (some-> actor-name actor->component-name prim/classname->class)]
    cls))
(>fdef actor-class [env actor-name]
  [::env ::actor-name => (s/nilable ::prim/component-class)])

(defn reset-actor-ident
  "Safely changes the ident of an actor.

  Makes sure ident is consistently reset and updates the actor class (if one is specified
  using `with-actor-class`)."
  [env actor ident]
  (let [new-actor             (any->actor-component-registry-key ident)
        actor->ident          (-> env
                                (asm-value ::actor->ident)
                                (assoc actor ident))
        ident->actor          (clojure.set/map-invert actor->ident)

        actor->ident-path     (asm-path env ::actor->ident)
        actor->component-path (conj (asm-path env ::actor->component-name) actor)
        ident->actor-path     (asm-path env ::ident->actor)]
    (-> env
      (assoc-in actor->ident-path actor->ident)
      (assoc-in ident->actor-path ident->actor)
      (cond->
        new-actor (assoc-in actor->component-path new-actor)))))
(>fdef reset-actor-ident [env actor ident]
  [::env ::alias ::fulcro-ident => ::env])

(defn assoc-aliased
  "Similar to clojure.core/assoc but works on UISM env and aliases."
  ([env alias new-value alias-2 value-2 & kv-pairs]
   (apply set-aliased-value env alias new-value
     alias-2 value-2 kv-pairs))
  ([env alias new-value]
   (set-aliased-value env alias new-value)))
(>fdef assoc-aliased
  ([env alias new-value alias-2 value-2 & kv-pairs]
   [::env ::alias any? ::alias any? (s/* any?) => ::env])
  ([env alias new-value]
   [::env ::alias any? => ::env]))

(defn update-aliased
  "Similar to clojure.core/update but works on UISM env and aliases."
  ([env k f]
   (assoc-aliased env k (f (alias-value env k))))
  ([env k f x]
   (assoc-aliased env k (f (alias-value env k) x)))
  ([env k f x y]
   (assoc-aliased env k (f (alias-value env k) x y)))
  ([env k f x y z]
   (assoc-aliased env k (f (alias-value env k) x y z)))
  ([env k f x y z & more]
   (assoc-aliased env k (apply f (alias-value env k) x y z more))))
(>fdef update-aliased
  ([env k f]
   [::env ::alias any? => ::env])
  ([env k f x]
   [::env ::alias any? any? => ::env])
  ([env k f x y]
   [::env ::alias any? any? any? => ::env])
  ([env k f x y z]
   [::env ::alias any? any? any? any? => ::env])
  ([env k f x y z & more]
   [::env ::alias any? any? any? any? (s/* any?) => ::env]))

(declare apply-action)

(defn dissoc-aliased
  "Similar to clojure.core/dissoc but works on UISM env and aliases."
  ([env] env)
  ([env alias]
   (when-not (nil? env)
     (let [path     (resolve-alias env alias)
           sub-path (butlast path)
           k        (last path)]
       (log/debug "Dissoc of aliased value" alias "on" (::asm-id env))
       (apply-action env #(update-in % sub-path dissoc k)))))
  ([env k & ks]
   (when-not (nil? env)
     (let [ret (dissoc-aliased env k)]
       (if ks
         (recur ret (first ks) (next ks))
         ret)))))
(>fdef dissoc-aliased
  ([env] [::env => ::env])
  ([env alias] [::env ::alias => ::env])
  ([env k & ks] [::env ::alias (s/* ::alias) => ::env]))

(defn integrate-ident
  "Integrate an ident into any number of aliases in the state machine.
  Aliases must point to a list of idents.

  The named parameters can be specified any number of times. They are:

  - append:  A keyword (alias) to a list in your app state where this new object's ident should be appended. Will not append
  the ident if that ident is already in the list.
  - prepend: A keyword (alias) to a list in your app state where this new object's ident should be prepended. Will not append
  the ident if that ident is already in the list."
  [env ident & named-parameters]
  (log/debug "Integrating" ident "on" (::asm-id env))
  (let [actions (partition 2 named-parameters)]
    (reduce (fn [env [command alias-to-idents]]
              (let [alias-value                 (alias-value env alias-to-idents)
                    already-has-ident-at-alias? (some #(= % ident) alias-value)]
                (case command
                  :prepend (if already-has-ident-at-alias?
                             env
                             (update-aliased env alias-to-idents #(into [ident] %)))
                  :append (if already-has-ident-at-alias?
                            env
                            (update-aliased env alias-to-idents (fnil conj []) ident))
                  (throw (ex-info "Unknown operation for integrate-ident: " {:command command :arg alias-to-idents})))))
      env actions)))
(>fdef integrate-ident
  [env ident & named-parameters]
  [::env ::fulcro-ident
   (s/* (s/cat :name #{:prepend :append} :param keyword?))
   => ::env])

(defn remove-ident
  "Removes an ident, if it exists, from an alias that points to a list of idents."
  [env ident alias-to-idents]
  (log/debug "Removing" ident "from" alias-to-idents "on" (::asm-id env))
  (let [new-list (fn [old-list]
                   (vec (filter #(not= ident %) old-list)))]
    (update-aliased env alias-to-idents new-list)))
(>fdef remove-ident
  [env ident alias-to-idents]
  [::env ::fulcro-ident ::alias => ::env])

(defn queue-mutations!
  [reconciler env]
  (let [queued-mutations (::queued-mutations env)]
    (doseq [{::keys [mutation-context] :as mutation-params} queued-mutations]
      (let [contextual-component (prim/ref->any reconciler (actor->ident env mutation-context))]
        (if (nil? contextual-component)
          (log/error "Cannot run mutation. No contextual component mounted for " mutation-context)
          (pm/pmutate! contextual-component mutation-delegate mutation-params))))
    nil))
(>fdef queue-mutations!
  [reconciler env]
  [::fulcro-reconciler ::env => nil?])

(defn queue-actor-load!
  "Internal implementation. Queue a load of an actor."
  [reconciler env actor-name component-class load-options]
  (let [actor-ident (actor->ident env actor-name)
        cls         (or component-class (actor-class env actor-name) (prim/react-type (prim/ref->any reconciler actor-ident)))]
    (log/debug "Starting actor load" actor-name "on" (::asm-id env))
    (if (nil? cls)
      (log/error "Cannot run load. Counld not derive Fulcro class (and none was configured) for " actor-name)
      (defer #(df/load reconciler actor-ident cls load-options)))
    nil))
(>fdef queue-actor-load!
  [reconciler env actor-name component-class load-options]
  [::fulcro-reconciler ::env ::actor-name (s/nilable ::prim/component-class) ::load-options => nil?])

(defn queue-normal-load!
  "Internal implementation. Queue a load."
  [reconciler query-key component-class load-options]
  (if (nil? query-key)
    (log/error "Cannot run load. query-key cannot be nil.")
    (do
      (log/debug "Starting load of" query-key)
      (defer #(df/load reconciler query-key component-class load-options))))
  nil)
(>fdef queue-normal-load!
  [reconciler query-key component-class load-options]
  [::fulcro-reconciler ::query-key (s/nilable ::prim/component-class) ::load-options => nil?])

(defn handle-load-error* [reconciler load-request]
  (let [{::keys [asm-id error-event error-data]} (some-> load-request :post-mutation-params)]
    (log/debug "Handling load error" asm-id ":" error-event)
    (if (and asm-id error-event)
      (defer
        #(prim/transact! reconciler [(trigger-state-machine-event (cond-> {::asm-id   asm-id
                                                                           ::event-id error-event}
                                                                    error-data (assoc ::event-data error-data)))]))
      (do
        (log/warn "A fallback occurred, but no event was defined by the client. Sending generic ::uism/load-error event.")
        (defer
          #(prim/transact! reconciler [(trigger-state-machine-event (cond-> {::asm-id   asm-id
                                                                             ::event-id ::load-error}))])))))
  nil)
(>fdef handle-load-error* [r req]
  [::fulcro-reconciler ::load-options => nil?])

(defmutation handle-load-error [_]
  (action [{:keys [reconciler load-request]}]
    (handle-load-error* reconciler load-request)))

(defn queue-loads! [reconciler env]
  (let [queued-loads (::queued-loads env)]
    (doseq [{::prim/keys [component-class]
             ::keys      [actor-name query-key load-options] :as load-params} queued-loads]
      (if actor-name                                        ; actor-centric load
        (queue-actor-load! reconciler env actor-name component-class load-options)
        (queue-normal-load! reconciler query-key component-class load-options))))
  nil)
(>fdef queue-loads! [reconciler env] [::fulcro-reconciler ::env => nil?])

(defn update-fulcro-state!
  "Put the evolved state-map from an env into a (Fulcro) state-atom"
  [{::keys [asm-id] :as env} state-atom]
  (let [next-state (when env (asm-value env ::active-state))]
    (when-let [new-fulcro-state (some-> (::state-map env)
                                  ;; GC state machine if it exited
                                  (cond->
                                    (= ::exit next-state) (update ::asm-id dissoc asm-id)))]
      (reset! state-atom new-fulcro-state)))
  nil)
(>fdef update-fulcro-state! [{::keys [asm-id] :as env} state-atom]
  [::env ::atom => nil?])

(defn set-timeout
  "Add a timeout named `timer-id` to the `env` that will send `event-id` with `event-data` event
   after `timeout` (in milliseconds) unless an event (i.e. some-event-id) occurs where a call
   to `(cancel-on-events some-event-id)` returns true.

   Setting a timeout on an existing timer-id will cancel the current one and start the new one.

   `cancel-on-events` is a predicate that will be passed an event ID on events. If it returns true
    on an event before the timeout fires, then the timeout will be auto-cancelled. If not specified, then
    it defaults to `(constantly false)`."
  ([env timer-id event-id event-data timeout]
   (set-timeout env timer-id event-id event-data timeout (constantly false)))
  ([env timer-id event-id event-data timeout cancel-on-events]
   (let [descriptor (cond-> {::timeout   timeout
                             ::timer-id  timer-id
                             ::js-timer  (with-meta {} {:timer true})
                             ::event-id  event-id
                             ::cancel-on (with-meta {} {:cancel-on cancel-on-events})}
                      event-data (assoc ::event-data event-data))]
     (update env ::queued-timeouts (fnil conj []) descriptor))))
(>fdef set-timeout
  ([env timer-id event-id event-data timeout]
   [::env ::timer-id ::event-id ::event-data pos-int? => ::env])
  ([env timer-id event-id event-data timeout cancel-on-events]
   [::env ::timer-id ::event-id ::event-data pos-int? ::cancel-fn => ::env]))

(defn clear-timeout!
  "Clear a scheduled timeout (if it has yet to fire).  Harmless to call if the timeout is gone. This call takes
  effect immediately (in terms of making sure the timeout does not fire)."
  [env timer-id]
  (log/debug "Clearing timeout " (::asm-id env) ":" timer-id)
  (let [{::keys [js-timer]} (asm-value env [::active-timers timer-id])
        real-js-timer (-> js-timer meta :timer)]
    (when real-js-timer
      (clear-js-timeout! real-js-timer))
    (-> env
      (update-in (asm-path env [::active-timers]) dissoc timer-id))))
(>fdef clear-timeout! [env timer-id]
  [::env ::timer-id => ::env])

(defn generic-event-handler
  "Returns an event handler that can process events according to a state machine
  ::uism/events definition of the current event/state in `env`.
  If a definition cannot be found then it returns nil."
  [original-env]
  (let [smdef            (lookup-state-machine original-env)
        current-state-id (asm-value original-env ::active-state)
        current-event    (::event-id original-env)
        {::keys [event-predicate handler target-state] :as event-def} (some-> smdef ::states (get current-state-id) ::events (get current-event))]
    (if event-def
      (fn [env]
        (if (or (nil? event-predicate) (and event-predicate (event-predicate env)))
          (let [env                (if handler (or (handler env) env) env)
                post-handler-state (-> env (asm-value ::active-state))
                state-changed?     (not= post-handler-state current-state-id)]
            (cond-> env
              (and (not state-changed?) target-state) (activate target-state)))
          ;; IMPORTANT: UNDO value changes if the predicate is disabled
          original-env))
      nil)))
(>fdef generic-event-handler [original-env]
  [::env => (s/nilable ::handler)])

(defn active-state-handler
  "Find the handler for the active state in the current env."
  [env]
  [::env => ::handler]
  (let [smdef         (lookup-state-machine env)
        current-state (asm-value env ::active-state)
        handler       (or
                        (get-in smdef [::states current-state ::handler])
                        (generic-event-handler env))]
    (if handler
      handler
      (let [{::keys [event-id]} env]
        (log/warn "UNEXPECTED EVENT: Did not find a way to handle event" event-id "in the current active state:" current-state)
        identity))))
(>fdef active-state-handler [env]
  [::env => ::handler])

(defn- ui-refresh-list
  "Returns a vector of things to refresh in Fulcro based on the final state of an active SM env."
  [env]
  (let [actor-idents (or (some-> env (get-in (asm-path env ::actor->ident)) vals vec) [])]
    actor-idents))
(>fdef ui-refresh-list
  [env]
  [::env => (s/coll-of futil/ident? :kind vector?)])

(defn- get-js-timer [env timer-id]
  (some-> (asm-value env [::active-timers timer-id]) ::js-timer meta :timer))
(>fdef get-js-timer [env timer-id]
  [::env ::timer-id => any?])


(defn schedule-timeouts!
  "INTERNAL: actually schedule the timers that were submitted during the event handler."
  [reconciler env]
  (let [{::keys [queued-timeouts asm-id]} env]
    (reduce
      (fn [env {::keys [timeout event-id event-data timer-id] :as descriptor}]
        (log/debug "Setting timeout" timer-id "on" asm-id "to send" event-id "in" timeout "ms")
        (let [current-timer (get-js-timer env timer-id)
              js-timer      (set-js-timeout! (fn []
                                               (log/debug "TIMEOUT on" asm-id "due to timer" timer-id "after" timeout "ms")
                                               (trigger! reconciler asm-id event-id (or event-data {}))) timeout)
              descriptor    (update-in descriptor [::js-timer] vary-meta assoc :timer js-timer)]
          (when current-timer
            (log/debug "Clearing old timer (new timer supercedes)")
            (clear-js-timeout! current-timer))
          (assoc-in env (asm-path env [::active-timers timer-id]) descriptor)))
      env
      queued-timeouts)))
(>fdef schedule-timeouts! [reconciler env]
  [::fulcro-reconciler ::env => ::env])

(defn clear-timeouts-on-event!
  "Processes the auto-cancel of events. This is a normal part of the internals, but can be used in handlers
  to simulate a *different* event than acutally occured for the purpose of clearing sets of timers that
  auto-cancel on other events than what occurred."
  [env event-id]
  (let [active-timers (asm-value env ::active-timers)]
    (reduce
      (fn [env timer-id]
        (let [cancel-predicate (some-> (get-in active-timers [timer-id ::cancel-on]) meta :cancel-on)]
          (when-not cancel-predicate
            (log/error "INTERNAL ERROR: Cancel predicate was nil for timer " timer-id))
          (if (and cancel-predicate (cancel-predicate event-id))
            (do
              (log/debug "Cancelling timer " timer-id "on" (::asm-id env) "due to event" event-id)
              (clear-timeout! env timer-id))
            env)))
      env
      (keys active-timers))))
(>fdef clear-timeouts-on-event! [env event-id]
  [::env ::event-id => ::env])

(declare trigger-state-machine-event!)

(s/def :fulcro/reconciler ::fulcro-reconciler)
(s/def :fulcro/state ::atom)
(s/def ::mutation-env (s/keys :req-un [:fulcro/state :fulcro/reconciler]))

(defn trigger-queued-events! [mutation-env queued-triggers refresh-list]
  (let [result
        (reduce (fn [refresh-list event]
                  (into refresh-list (trigger-state-machine-event! mutation-env event)))
          refresh-list
          queued-triggers)]
    result))
(>fdef trigger-queued-events! [mutation-env queued-triggers refresh-list]
  [::mutation-env (? ::queued-triggers) ::refresh-vector => ::refresh-vector])

(defn trigger-state-machine-event!
  "IMPLEMENTATION DETAIL. Low-level implementation of triggering a state machine event. Does no direct interaction with
  Fulcro UI refresh.  Use `trigger!` instead.

  - `env` - A fulcro mutation env, containing at least the state atom and optionally the ref of the
    component that was the source of the event.
  - params - The parameters for the event

  Returns a vector of actor idents that should be refreshed."
  [{:keys [reconciler state ref] :as mutation-env} {::keys [event-id event-data asm-id] :as params}]
  (when-not (get-in @state [::asm-id asm-id])
    (log/error "Attemped to trigger event " event-id "on state machine" asm-id ", but that state machine has not been started (call begin! first)."))
  (let [sm-env       (state-machine-env @state ref asm-id event-id event-data)
        handler      (active-state-handler sm-env)
        valued-env   (apply-event-value sm-env params)
        handled-env  (handler (assoc valued-env ::fulcro-reconciler reconciler))
        final-env    (as-> (or handled-env valued-env) e
                       (clear-timeouts-on-event! e event-id)
                       (schedule-timeouts! reconciler e))
        refresh-list (ui-refresh-list final-env)]
    (queue-mutations! reconciler final-env)
    (queue-loads! reconciler final-env)
    (update-fulcro-state! final-env state)
    (trigger-queued-events! mutation-env (::queued-triggers final-env) refresh-list)))
(>fdef trigger-state-machine-event!
  [mutation-env params]
  [::mutation-env ::trigger-descriptor => ::refresh-vector])

(defn trigger
  "Trigger an event on another state machine.

  `env` - is the env in a state machine handler
  `state-machine-id` - The ID of the state machine you want to trigger an event on.
  `event` - The event ID you want to send.
  `event-data` - A map of data to send with the event

  Returns the updated env.  The actual event will not be sent until this handler finishes."
  ([env state-machine-id event] (trigger env state-machine-id event {}))
  ([env state-machine-id event event-data]
   (update env ::queued-triggers (fnil conj []) {::asm-id     state-machine-id
                                                 ::event-id   event
                                                 ::event-data event-data})))
(>fdef trigger
  ([env state-machine-id event] [::env ::asm-id ::event-id => ::env])
  ([env state-machine-id event event-data] [::env ::asm-id ::event-id ::event-data => ::env]))

(defmutation trigger-state-machine-event
  "Mutation: Trigger an event on an active state machine"
  [{::keys [event-id event-data asm-id] :as params}]
  (action [{:keys [reconciler] :as env}]
    (let [to-refresh (trigger-state-machine-event! env params)]
      ;; IF this is triggered from a post-mutation event and more than a request-animation-frame amount of time has
      ;; elapsed THEN we have NO access to doing remoting (from post mutations), but the call to queue will end up
      ;; running a React render on THIS thread, and any mutations/loads within React lifecycle methods will be lost.
      (defer (fn [] (fulcro.client.util/force-render reconciler to-refresh))))
    true))

(defn set-string!
  "Similar to Fulcro's set-string, but it sets the string on an active state machine's data alias.
  event-or-string can be a string or a React DOM onChange event.

  The incoming `event-data` to your handler will include `::uism/alias` and `:value` (if you care to do anything
  with the value change event).

  NOTE: Generates a ::uism/value-changed event. If you're state machine is implemented with the events
  structure that allows an event-predicate, then this set will be ignored if the current state's event-predicate
  returns false."
  [this active-state-machine-id alias event-or-string]
  (let [value (if (string? event-or-string)
                event-or-string
                (or (some-> event-or-string .-target .-value) ""))]
    (trigger! this active-state-machine-id ::value-changed {::alias alias
                                                            :value  value})))

(defn set-value!
  "Similar to Fulcro's set-value, but it sets the raw value on an active state machine's data alias.

  The incoming `event-data` to your handler will include `::uism/alias` and `:value` (if you care to do anything
  with the value change event).

  NOTE: Generates a ::uism/value-changed event. If you're state machine is implemented with the events
  structure that allows an event-predicate, then this set will be ignored if the current state's event-predicate
  returns false."
  [this active-state-machine-id alias value]
  (trigger! this active-state-machine-id ::value-changed {::alias alias
                                                          :value  value}))

(defmutation begin
  "Mutation to begin a state machine. Use `begin!` instead."
  [{::keys [asm-id event-data] :as params}]
  (action [{:keys [reconciler state] :as env}]
    (swap! state (fn [s]
                   (-> s
                     (assoc-in [::asm-id asm-id] (new-asm params)))))
    (let [to-refresh (trigger-state-machine-event! env (cond-> {::event-id   ::started
                                                                ::asm-id     asm-id
                                                                ::event-data {}}
                                                         event-data (assoc ::event-data event-data)))]
      (fulcro.client.util/force-render reconciler to-refresh))))

(mi/declare-mutation begin `begin)

(defn derive-actor-idents
  "Generate an actor->ident map."
  [actors]
  (into {}
    ;; v can be an ident, component, or component class
    (keep (fn [[actor-id v]]
            (cond
              (and (prim/component? v) (-> (prim/get-ident v) second))
              [actor-id (prim/get-ident v)]

              (and (prim/component-class? v) (-> (prim/get-ident v {}) second))
              [actor-id (prim/get-ident v {})]

              (futil/ident? v) [actor-id v]
              :otherwise (do
                           (log/error "The value given for actor" actor-id "had (or was) an invalid ident:" v)
                           nil))))
    actors))
(>fdef derive-actor-idents [actors] [(s/map-of ::actor-name (s/or
                                                              :ident ::fulcro-ident
                                                              :component prim/component?
                                                              :class prim/component-class?)) => ::actor->ident])



(defn derive-actor-components
  "Calculate the map from actor names to the Fulcro component registry names that represent those actors."
  [actors]
  (into {}
    ;; v can be an ident, component, or component class
    (keep (fn [[actor-id v]]
            (when-let [k (any->actor-component-registry-key v)]
              [actor-id k])))
    actors))
(>fdef derive-actor-components [actors] [(s/map-of ::actor-name (s/or
                                                                  :ident ::fulcro-ident
                                                                  :component prim/component?
                                                                  :class prim/component-class?)) => ::actor->component-name])

(defn begin!
  "Install and start a state machine.

  this - A UI component or reconciler
  machine - A state machine defined with defstatemachine
  instance-id - An ID by which you will refer to this active instance.
  actors - A map of actor-names -> The ident, class, or react instance that represent them in the UI. Raw idents do not support SM loads.
  started-event-data - Data that will be sent with the ::uism/started event as ::uism/event-data"
  ([this machine instance-id actors]
   (begin! this machine instance-id actors {}))
  ([this machine instance-id actors started-event-data]
   (let [actors->idents          (derive-actor-idents actors)
         actors->component-names (derive-actor-components actors)]
     (log/debug "begin!" instance-id)
     (prim/transact! this [(begin {::asm-id                instance-id
                                   ::state-machine-id      (::state-machine-id machine)
                                   ::event-data            started-event-data
                                   ::actor->component-name actors->component-names
                                   ::actor->ident          actors->idents})]))))
(>fdef begin!
  ([this machine instance-id actors]
   [(s/or :c ::prim/component :r ::fulcro-reconciler) ::state-machine-definition ::asm-id (s/map-of ::actor-name any?) => any?])
  ([this machine instance-id actors started-event-data]
   [(s/or :c ::prim/component :r ::fulcro-reconciler) ::state-machine-definition ::asm-id (s/map-of ::actor-name any?) ::event-data => any?]))

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
(s/def ::mutation-descriptor (s/keys :req [::mutation-context ::mutation]
                               :opt [::pm/target ::ok-event ::ok-data ::error-event ::error-data
                                     ::pm/returning ::mutation-remote]))
(s/def ::mutation-remote keyword?)
(s/def ::queued-mutations (s/coll-of ::mutation-descriptor))

(defn compute-target
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
(>fdef compute-target
  [env target-options]
  [::env (s/keys :opt [::pm/target ::target-actor ::target-alias]) => (s/nilable vector?)])

(let [mtrigger! (fn mutation-trigger* [{:keys [reconciler state]} actor-ident asm-id event data]
                  (let [response   (get-in @state (conj actor-ident ::pm/mutation-response))
                        event-data (merge {} data response)]
                    (defer
                      #(prim/transact! reconciler actor-ident [(trigger-state-machine-event {::asm-id     asm-id
                                                                                             ::event-id   event
                                                                                             ::event-data event-data})]))))]
  (defmethod m/mutate `mutation-delegate [{:keys [reconciler state] :as env} _
                                          {::keys [asm-id ok-event error-event mutation
                                                   mutation-context ok-data error-data mutation-remote] :as mp}]
    ;; mutation can be run for figuring out remote
    (if (or (empty? @state) (empty? mp))
      {(or mutation-remote :remote) (pm/pessimistic-mutation env)}
      (let [sm-env      (state-machine-env @state nil asm-id ok-event ok-data)
            actor-ident (actor->ident sm-env mutation-context)
            to-refresh  (ui-refresh-list sm-env)
            abort-id    (:abort-id mp)
            fixed-env   (-> env
                          (assoc :ref actor-ident)
                          (cond-> abort-id (update :ast m/with-abort-id abort-id))
                          (update :ast assoc
                            :key mutation
                            :dispatch-key mutation
                            :params (dissoc mp ::ok-event ::error-event ::mutation
                                      ::mutation-context ::ok-data ::error-data
                                      ::mutation-remote ::asm-id)))]
        (cond-> {:refresh                     to-refresh
                 (or mutation-remote :remote) (pm/pessimistic-mutation fixed-env)}
          ok-event (assoc :ok-action (fn []
                                       (log/debug "Remote mutation " mutation "success")
                                       (mtrigger! fixed-env actor-ident asm-id ok-event ok-data)))
          error-event (assoc :error-action (fn []
                                             (log/debug "Remote mutation " mutation "error")
                                             (mtrigger! fixed-env actor-ident asm-id error-event error-data))))))))

(defn trigger-remote-mutation
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
  (let [target              (compute-target env options-and-params)
        asm-id              (::asm-id env)
        mutation-sym        (mi/mutation-symbol mutation {})
        mutation-descriptor (-> options-and-params
                              (dissoc ::target-actor ::target-alias ::pm/target)
                              (assoc ::asm-id asm-id ::mutation mutation-sym ::mutation-context actor)
                              (cond->
                                (seq target) (assoc ::pm/target target)))]
    (update env ::queued-mutations (fnil conj []) mutation-descriptor)))
(>fdef trigger-remote-mutation
  [env actor mutation options-and-params]
  [::env ::actor-name
   (s/or :sym ::mutation :decl ::mutation-decl)
   (s/keys :opt [::pm/returning ::pm/target ::target-actor
                 ::target-alias ::ok-event ::error-event ::ok-data ::error-data ::mutation-remote])
   => ::env])

;; ================================================================================
;; I/O: Load integration
;; ================================================================================

(s/def ::load-options map?)
(s/def ::query-key (s/or :key keyword? :ident ::fulcro-ident))
(s/def ::load (s/keys :opt [::query-key ::prim/component-class ::load-options]))
(s/def ::queued-loads (s/coll-of ::load))
(s/def ::post-event ::event-id)
(s/def ::post-event-params map?)
(s/def ::fallback-event-params map?)

(defn convert-load-options
  "INTERNAL: Convert SM load options into Fulcro load options."
  [env options]
  (let [{::keys [post-event post-event-params fallback-event fallback-event-params target-actor target-alias]} options
        {:keys [marker]} options
        marker  (if (nil? marker) false marker)             ; force marker to false if it isn't set
        {::keys [asm-id]} env
        options (-> (dissoc options ::post-event ::post-event-params ::fallback-event ::fallback-event-params ::prim/component-class
                      ::target-alias ::target-actor)
                  (assoc :marker marker :abort-id asm-id :fallback `handle-load-error :post-mutation-params (merge post-event-params {::asm-id asm-id}))
                  (cond->
                    (or target-actor target-alias) (assoc :target (compute-target env options))
                    post-event (->
                                 (assoc :post-mutation `trigger-state-machine-event)
                                 (update :post-mutation-params assoc ::event-id post-event))
                    post-event-params (update :post-mutation-params assoc ::event-data post-event-params)
                    ;; piggieback the fallback params and event on post mutation data, since it is the only thing we can see
                    fallback-event (update :post-mutation-params assoc ::error-event fallback-event)
                    fallback-event-params (update :post-mutation-params assoc ::error-data fallback-event-params)))]
    options))
(>fdef convert-load-options [env options]
  [::env (s/keys :opt [::post-event ::post-event-params ::fallback-event ::fallback-event-params]) => map?])

(defn load
  "Identical API to fulcro's data fetch `load`, but using a handle `env` instead of a component/reconciler.
   Adds the load request to then env which will be sent to Fulcro as soon as the handler finishes.

   The 3rd argument can be a Fulcro class or a UISM actor name that was registered with `begin!`.

  The `options` are as in Fulcro's load, with the following additional keys for convenience:

  `::uism/post-event`:: An event to send when the load is done (instead of calling a mutation)
  `::uism/post-event-params`:: Extra parameters to send as event-data on the post-event.
  `::uism/fallback-event`:: The event to send if the load triggers a fallback.
  `::uism/fallback-event-params`:: Extra parameters to send as event-data on a fallback.
  `::uism/target-actor`:: Set target to a given actor's ident.
  `::uism/target-alias`:: Set load target to the path defined by the given alias.

   NOTE: In general a state machine should declare an actor for items in the machine and use `load-actor` instead of
   this function so that the state definitions themselves need not be coupled (via code) to the UI."
  ([env key-or-ident component-class-or-actor-name]
   (load env key-or-ident component-class-or-actor-name {}))
  ([env key-or-ident component-class-or-actor-name options]
   (let [options (convert-load-options env options)
         class   (if (s/valid? ::actor-name component-class-or-actor-name)
                   (actor-class env component-class-or-actor-name)
                   component-class-or-actor-name)]
     (update env ::queued-loads (fnil conj []) (cond-> {}
                                                 class (assoc ::prim/component-class class)
                                                 key-or-ident (assoc ::query-key key-or-ident)
                                                 options (assoc ::load-options options))))))
(>fdef load
  ([env k actor-or-class] [::env ::query-key (s/or :a ::actor-name :c ::prim/component-class) => ::env])
  ([env k actor-or-class options] [::env ::query-key (s/or :a ::actor-name :c ::prim/component-class) ::load-options => ::env]))

(defn load-actor
  "Load (refresh) the given actor. If the actor *is not* on the UI, then you *must* specify
   `:fulcro.client.primitives/component-class` in the `options` map.

   options can contain the normal `df/load` parameters, and also:

  `::prim/component-class` - The defsc name of the component to use for normalization and query. Only needed if the
    actor was not declared using a Fulcro component or component class.
  `::uism/post-event`:: An event to send when the load is done (instead of calling a mutation)
  `::uism/post-event-params`:: Extra parameters to send as event-data on the post-event.
  `::uism/fallback-event`:: The event to send if the load triggers a fallback.
  `::uism/fallback-event-params`:: Extra parameters to send as event-data on a fallback.

   Adds a load request to then env which will be sent to Fulcro as soon as the handler finishes."
  ([env actor-name]
   (load-actor env actor-name {}))
  ([env actor-name {::prim/keys [component-class] :as options}]
   (let [options (convert-load-options env options)]
     (update env ::queued-loads (fnil conj []) (cond-> {::actor-name   actor-name
                                                        ::load-options options}
                                                 component-class (assoc ::prim/component-class component-class))))))
(>fdef load-actor
  ([env actor-name] [::env ::actor-name => ::env])
  ([env actor-name {::prim/keys [component-class] :as options}] [::env ::actor-name ::load-options => ::env]))

(defn apply-action
  "Run a mutation helper function (e.g. a fn of Fulcro state)."
  [env mutation-helper & args]
  [::env fn? (s/* any?) => ::env]
  (log/debug "Applying mutation helper to state of" (::asm-id env))
  (apply update env ::state-map mutation-helper args))
(>fdef apply-action [env mutation-helper & args] [::env fn? (s/* any?) => ::env])

(defn get-active-state
  "Get the name of the active state for an active state machine using a component."
  [this asm-id]
  (let [state-map (if (prim/reconciler? this)
                    (some-> this prim/app-state deref)
                    (prim/component->state-map this))]
    (some-> state-map
      ::asm-id
      (get asm-id)
      ::active-state)))
(>fdef get-active-state [this asm-id]
  [(s/or :c prim/component? :r ::fulcro-reconciler) ::asm-id => (? keyword?)])
