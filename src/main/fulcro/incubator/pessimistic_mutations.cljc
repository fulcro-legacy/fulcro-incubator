(ns fulcro.incubator.pessimistic-mutations
  "Support for easily defining mutations that automatically update components based on the status and result of a mutation.
  Includes support for a loading, error, and complete status, and makes writing consistent UIs around pessimistic
  mutation (instead of optimistic) easier."
  #?(:cljs (:require-macros fulcro.incubator.pessimistic-mutations))
  (:require
    [fulcro.incubator.db-helpers :as db.h]
    [fulcro.client.mutations :as mutations]
    [fulcro.client.primitives :as fp]
    [fulcro.client.data-fetch :as fetch]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]))

;; a safe way to save component in app state
(defn- response-component [component] (with-meta {} {:component component}))
(defn- get-response-component [response] (-> response :component meta :component))

(defn- transform-remote
  "A helper function that you should not use. "
  [env ast]
  (let [ast (if (true? ast) (:ast env) ast)]
    (if-let [component (some-> ast :query meta :component)]
      (db.h/swap-entity! env assoc-in [::mutation-response :component] (response-component component)))
    (-> ast
      (cond-> (:query ast) (update :query vary-meta dissoc :component))
      (mutations/with-target (conj (:ref env) ::mutation-response-swap)))))

#?(:clj
   (defn- gen-pessimistic-mutation [sym arglist forms]
     (let [sym       sym
           ok-sym    (with-meta (symbol (str sym "-ok")) (meta sym))
           error-sym (with-meta (symbol (str sym "-error")) (meta sym))
           {[pre]     'pre-action
            [action]  'action
            [error]   'error-action
            [refresh] 'refresh
            remotes   nil} (group-by (fn [x] (#{'action 'error-action 'pre-action 'refresh} (first x))) forms)
           refresh   (if refresh [refresh])
           remotes   (->> remotes
                       (mapv (fn [[s args & forms]]
                               (list s ['env]
                                 `(let [~(first args) ~'env]
                                    (transform-remote ~'env (do ~@forms)))))))
           action    (or action '(action [_] nil))
           pre'      (some-> pre vec (assoc 0 'action) (->> (apply list)))
           initial   (if pre (into [pre'] remotes) remotes)]
       `(do
          (mutations/defmutation ~sym ~arglist ~@initial)
          (mutations/defmutation ~ok-sym ~arglist ~action ~@refresh)
          ~(if error `(mutations/defmutation ~error-sym ~arglist ~(-> error next (conj 'action))))))))

#?(:clj
   (defmacro defpmutation
     "Defines a pessimistic mutation. This is adapted to work with ident based pessimist mutation
     system. This works similar to the normal `defmutation`, but instead of
     doing the optimistic update right after the action, it delays to when the response
     is success from the remote. If the remote fails, the UI change is not going to be
     applied."
     [sym arglist & forms]
     (gen-pessimistic-mutation sym arglist forms)))

#?(:clj
   (s/fdef defpmutation
     :args (s/cat :sym symbol? :args vector? :forms (s/+ list?))))

(defn mutation-response
  "Retrieves the mutation response from `this` component.  Can also be used against the state-map with an ident."
  ([this]
   (if (fp/component? this)
     (mutation-response (-> this fp/get-reconciler fp/app-state deref) (fp/props this))
     (-> this ::mutation-response)))
  ([state props]
   (let [response (-> props ::mutation-response)]
     (if (fulcro.util/ident? response) (get-in state response) response))))

(defn mutation-loading?
  "Checks this props of `this` component to see if a mutation is in progress."
  [this]
  (let [props (cond-> this (fp/component? this) fp/props)]
    (-> props ::mutation-response (fetch/loading?))))

(defn mutation-error?
  "Is the mutation in error. This is detected by looking for ::mutation-errors in the ::mutation-response (map) returned by the mutation."
  ([this]
   (-> (mutation-response this) (contains? ::mutation-errors)))
  ([state props]
   (-> (mutation-response state props) (contains? ::mutation-errors))))

(defn get-mutation
  "Runs the side-effect-free multimethod for the given (client) mutation and returns a map describing the mutation:

  {:action (fn [env] ...)
   :remote ...}"
  [env k p]
  (when-let [m (get (methods mutations/mutate) k)]
    (m env k p)))

(defn call-mutation-action
  "Call a Fulcro client mutation action (defined on the multimethod fulcro.client.mutations/mutate). This
  runs the `action` section of the mutation and returns its value."
  [env k p]
  (when-let [h (-> (get-mutation env k p) :action)]
    (h)))

(s/def ::mutation-response (s/keys))

(mutations/defmutation mutation-network-error
  "INTERNAL USE mutation."
  [{::keys [ref ] :as p}]
  (action [env]
    (db.h/swap-entity! (assoc env :ref ref) assoc ::mutation-response
      (-> p
        (dissoc ::ref)
        (assoc ::fp/error "Network error"
               ::mutation-errors :network-error)))
    nil))

(mutations/defmutation start-pmutation
  "INTERNAL USE mutation."
  [_]
  (action [env]
    (db.h/swap-entity! env assoc ::mutation-response {:fulcro.client.impl.data-fetch/type :loading})
    nil))

(mutations/defmutation finish-pmutation
  "INTERNAL USE mutation."
  [{:keys [ok-mutation error-mutation input]}]
  (action [env]
    (js/console.log :FINISH)
    (let [{:keys [state ref reconciler]} env
          error-marker (select-keys input #{::error-marker})
          {::keys [mutation-response mutation-response-swap] :as props} (get-in @state ref)]
      (if (mutation-error? @state (set/rename-keys props {::mutation-response-swap ::mutation-response}))
        (do
          (db.h/swap-entity! env assoc ::mutation-response (merge mutation-response mutation-response-swap error-marker))
          (call-mutation-action env error-mutation input))
        (do
          (when-let [component (get-response-component mutation-response)]
            (fp/merge-component! reconciler component mutation-response-swap))

          (when (mutation-loading? props) (db.h/swap-entity! env dissoc ::mutation-response))

          (call-mutation-action env ok-mutation input)))
      (db.h/swap-entity! env dissoc ::mutation-response-swap))))

(defn pmutate!
  "Run a pmutation defined by `defpmutation`.

  this - The component whose ident will be used for status reporting on the progress of the mutation.
  mutation - The symbol of the mutation you want to run.
  params - The parameter map for the mutation

  The following special keys can be included in `params` to augment how it works:

  - `:fulcro.incubator.pessimistic-mutations/error-marker any-value` -
    This k/v pair will be in included in the ::mutation-response if is an error. This allows you to distinguish
    errors among components that share an ident (e.g. one component causes a mutation error, but all with that ident update)."
  [this mutation params]
  (let [ok-mutation    (symbol (str mutation "-ok"))
        error-mutation (symbol (str mutation "-error"))]
    (fp/ptransact! this `[(start-pmutation {})
                          ~(list mutation params)
                          (fulcro.client.data-fetch/fallback {:action          mutation-network-error
                                                              ::ref            ~(fp/get-ident this)})
                          (finish-pmutation ~{:ok-mutation    ok-mutation
                                              :error-mutation error-mutation
                                              :input          params})])))
