(ns fulcro.incubator.pessimistic-mutations
  "Support for easily defining mutations that automatically update components based on the status and result of a mutation.
  Includes support for a loading, error, and complete status, and makes writing consistent UIs around pessimistic
  mutation (instead of optimistic) easier."
  #?(:cljs (:require-macros [fulcro.incubator.pessimistic-mutations]))
  (:require
    [fulcro.incubator.db-helpers :as db.h]
    [fulcro.incubator.mutation-interface :as mi]
    [fulcro.client.mutations :as mutations :refer [defmutation]]
    [fulcro.client.primitives :as fp]
    [fulcro.client.impl.data-targeting :as data-targeting]
    [fulcro.logging :as log]
    [clojure.spec.alpha :as s]
    [fulcro.client.primitives :as prim]
    [fulcro.client.data-fetch :as df]
    [clojure.set :as set]))

(s/def ::returning ::fp/component-class)
(s/def ::target ::df/target)

(def error-states #{:api-error :network-error})

(defn pessimistic-mutation
  "You must call this function in the remote of mutations that are used with `pmutate!`.

  (defmutation x [_]
    (remote [env] (pessimistic-remote env)))

  NOTES: You *must not* compose this with Fulcro's `returning` or `with-target`.
  You should instead use the special keys of `pmutate`'s params.
  "
  [{:keys [ast ref]}]
  (when (:query ast)
    (log/error "You should not use mutation joins (returning) with `pmutate!`. Use the params of `pmutate!` instead."))
  (some-> ast
    (update :params dissoc ::returning ::target)
    (cond-> (:query ast) (update :query vary-meta dissoc :component))
    (mutations/with-target (conj ref ::mutation-response-swap))
    (vary-meta assoc ::pessimistic-mutation true)))

(defn- pessimistic-mutation?
  [ast]
  (-> ast meta ::pessimistic-mutation))

(defn mutation-response
  "Retrieves the mutation response from `this-or-props` (a component or props).  Can also be used against the state-map with an ident."
  ([this-or-props]
   (if (fp/component? this-or-props)
     (mutation-response (-> this-or-props fp/get-reconciler fp/app-state deref) (fp/props this-or-props))
     (-> this-or-props ::mutation-response)))
  ([state props]
   (let [response (-> props ::mutation-response)]
     (if (fulcro.util/ident? response) (get-in state response) response))))

(defn- mutation-status
  ([state props]
   (let [response (mutation-response state props)]
     (-> response ::status)))
  ([this]
   (-> (mutation-response this) ::status)))

(defn mutation-loading?
  "Checks this props of `this` component to see if a mutation is in progress."
  [this]
  (= :loading (mutation-status this)))

(defn mutation-error?
  "Is the mutation in error. This is detected by looking for ::mutation-errors in the ::mutation-response (map) returned by the mutation."
  ([this]
   (contains? error-states (mutation-status this)))
  ([state props]
   (contains? error-states (mutation-status state props))))

(defn get-mutation
  "Runs the side-effect-free multimethod for the given (client) mutation and returns a map describing the mutation:

  {:action (fn [env] ...)
   :remote ...}"
  [env k p]
  (try
    (when-let [m (get (methods mutations/mutate) k)]
      (m env k p))
    (catch #?(:clj Exception :cljs :default) e
      (log/error "Unable to read mutation. Some features of pmutate! may fail.  You should check your remote(s) to make sure they tolerate an empty env." e))))

(defn call-mutation-action
  "Call a Fulcro client mutation action (defined on the multimethod fulcro.client.mutations/mutate). This
  runs the `action` (or `custom-action`) section of the mutation and returns its value."
  ([custom-action env k p]
   (when-let [h (-> (get-mutation env k p) (get (keyword (name custom-action))))]
     (h)))
  ([env k p]
   (call-mutation-action :action env k p)))

(mutations/defmutation mutation-network-error
  "INTERNAL USE mutation."
  [{:keys  [error params]
    ::keys [ref] :as p}]
  (action [env]
    (let [low-level-error (some-> error first second :fulcro.client.primitives/error)
          {::keys [key]} params]
      (db.h/swap-entity! (assoc env :ref ref) assoc ::mutation-response-swap
        (cond-> (dissoc p ::ref :error :params)
          :always (assoc ::status :hard-error)
          key (assoc ::key key)
          low-level-error (assoc ::low-level-error low-level-error))))
    nil))

(mutations/defmutation start-pmutation
  "INTERNAL USE mutation."
  [{::keys [key target]}]
  (action [{:keys [state] :as env}]
    (let [loading-marker {::status :loading
                          ::key    key}]
      (db.h/swap-entity! env assoc ::mutation-response {::status :loading
                                                        ::key    key})
      (when (and (not (data-targeting/multiple-targets? target)) (vector? target) (map? (get-in @state target)))
        (swap! state assoc-in (conj target ::mutation-response) loading-marker)))
    nil))

(mutations/defmutation finish-pmutation
  "INTERNAL USE mutation."
  [{:keys [mutation params]}]
  (action [env]
    (let [{:keys [state ref reconciler]} env
          {::keys [key target returning]} params
          {::keys [mutation-response-swap]} (get-in @state ref)
          {::keys [status]} mutation-response-swap
          hard-error? (= status :hard-error)
          api-error?  (contains? mutation-response-swap ::mutation-errors)
          had-error?  (or hard-error? api-error?)]
      (if had-error?
        (do
          (db.h/swap-entity! env assoc ::mutation-response (merge {::status :api-error} mutation-response-swap {::key key}))
          (call-mutation-action :error-action env mutation params))
        (do
          ;; so the ok action can see it
          (db.h/swap-entity! env (fn [s]
                                   (-> s
                                     (dissoc ::mutation-response-swap)
                                     (assoc ::mutation-response (merge mutation-response-swap {::key key})))))
          (when returning
            (fp/merge-component! reconciler returning mutation-response-swap))
          (when target
            (if-let [return-value-ident (and target returning (fp/get-ident returning mutation-response-swap))]
              (do
                (when (nil? (second return-value-ident))
                  (log/warn "Targeted value of type " returning " did not generate a valid ident from the server return value: " mutation-response-swap))
                (swap! (:state env) data-targeting/process-target return-value-ident target false))
              (swap! (:state env) data-targeting/process-target (conj ref ::mutation-response-swap) target false))
            (when (get-in @state (conj target ::mutation-response))
              (swap! state update-in target dissoc ::mutation-response)))
          (call-mutation-action :ok-action env mutation params)
          (db.h/swap-entity! env dissoc ::mutation-response)))
      (db.h/swap-entity! env dissoc ::mutation-response-swap))))

(def ^:private fake-env {:state (atom {}) :parser (constantly {}) ; to help keep remotes from crashing
                         :ast   (prim/query->ast1 ['(noop)])})

;; bug in fulcro spec make it untestable unless I wrapped this :(
(defn- get-ident [c] (fp/get-ident c))

(defn- pmutation->ptransaction
  [this mutation params]
  (let [mutation         (mi/mutation-symbol mutation params)
        declared-refresh (:refresh (get-mutation fake-env mutation params))
        base-tx          `[(start-pmutation ~params)
                           ~(list mutation params)
                           (fulcro.client.data-fetch/fallback {:action mutation-network-error
                                                               :params ~params
                                                               ::ref   ~(get-ident this)})
                           (finish-pmutation ~{:mutation mutation :params params})]
        tx               (cond-> base-tx
                           (vector? declared-refresh) (into declared-refresh))]
    tx))

(defn pmutate!
  "Run a pmutation defined by `defpmutation`.

  this - The component whose ident will be used for status reporting on the progress of the mutation.
  mutation - The symbol of the mutation you want to run OR the mutation declaration name (using mutation-interface).
  params - The parameter map for the mutation.

  The following special keys can be included in `params` to augment how it works:

  - `::pm/key any-value` -
    This k/v pair will be in included in the ::mutation-response at all times. This allows you to distinguish
    among components that share an ident (e.g. one component causes a mutation error, but all with that ident update).
  - `::pm/target - The target for the mutation response (identical to `data-fetch/load`'s target parameter, including support
  for multiple).
  - `::pm/returning` - The component class of the return type, for normalization. If not specified then target will
  not be honored and no merge of the response will remain (only detect loading/errors of mutation).
  "
  [this mutation params]
  (let [tx (pmutation->ptransaction this mutation params)]
    (fp/ptransact! this tx)))

(defn pmutation?
  "Returns true if the given mutation (symbol or mutation declaration) has a signature that looks like a pmutation. That
  is to say it has an `ok-action` or `error-action` or returns `pessimistic-mutation` from one of its remotes. This function
  will issue a warning if the given mutation uses extended actions, but *fails* to return the proper `pessimistic-mutation`
  as well."
  [legal-remotes mutation params]
  (let [mutation        (mi/mutation-symbol mutation params)
        special-actions #{:ok-action :error-action}
        mutation-map    (get-mutation fake-env mutation params)
        pactions?       (set/subset? (keys mutation-map) special-actions)
        premote?        (boolean (some (fn [remote] (pessimistic-mutation? (get mutation-map remote))) legal-remotes))]
    (when (and pactions? (not premote?))
      (log/error (str "ERROR: " mutation "has an ok-action or error-action, but does *not* use `pessimistic-mutation` on any remote.")))
    ;; technically it will only be properly processed as a pmutate! if it has a premote.
    premote?))

(defn mixed-tx->ptransaction
  "Convert a tx that is a mix of normal mutations and pmutations into a tx that can be safely run with fulcro.primitives/ptransact!"
  [this tx]
  (let [reconciler    (prim/get-reconciler this)
        legal-remotes (some-> reconciler :config :remotes set)]
    (into []
      (mapcat
        (fn [element]
          (let [[msym params] (when (list? element) element)]
            (if (pmutation? legal-remotes msym params)
              (pmutation->ptransaction this msym params)
              [element]))))
      tx)))

(defn ptransact!
  "Just like Fulcro `ptransact!`, except it auto-detects `pmutate!` mutations and expands them to the proper form, allowing
  you to compose any kind of mutation together into a single, functioning transaction:

  ```
  (pi/ptransact! this `[(local-mutation) (normal-remote-mutation) (pmutate-mutation) (local-mutation)])
  ```
  "
  ([this ref tx] (prim/ptransact! this ref (mixed-tx->ptransaction this tx)))
  ([this tx] (prim/ptransact! this (mixed-tx->ptransaction this tx))))
