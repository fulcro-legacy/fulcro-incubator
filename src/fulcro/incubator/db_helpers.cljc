(ns fulcro.incubator.db-helpers
  #?(:cljs (:require-macros [fulcro.incubator.db-helpers]))
  (:require [com.wsscode.pathom.core]
            [clojure.spec.alpha :as s]
            [fulcro.client.mutations :as mutations]
            [fulcro.client.primitives :as fp]
            [fulcro.client.data-fetch :as fetch]
            [clojure.set :as set]))

(defn- fulcro-ident? [x]
  (and (vector? x)
       (= 2 (count x))
       (keyword? (first x))))

(defn query-component
  "Run a query against a component with ident. If you provide a path on
  focus-path, only that path will be queried, and the result will be pulled
  from the edge of the path."
  ([this]
   (let [component (fp/react-type this)
         ref       (fp/get-ident this)
         state     (-> this fp/get-reconciler fp/app-state deref)
         query     (fp/get-query component)]
     (fp/db->tree query (get-in state ref) state)))
  ([this focus-path]
   (let [component (fp/react-type this)
         ref       (fp/get-ident this)
         state     (-> this fp/get-reconciler fp/app-state deref)
         query     (fp/focus-query (fp/get-query component) focus-path)]
     (-> (fp/db->tree query (get-in state ref) state)
         (get-in focus-path)))))

(defn swap-entity! [{:keys [state ref]} & args]
  "Swap something, starts on the current ref path."
  (apply swap! state update-in ref args))

(defn resolve-path [state path]
  "Walks a db path, when find an ident it resets the path to that ident. Use to realize paths of relations."
  (loop [[h & t] path
         new-path []]
    (if h
      (let [np (conj new-path h)
            c  (get-in state np)]
        (if (fulcro-ident? c)
          (recur t c)
          (recur t (conj new-path h))))
      new-path)))

(defn swap-in! [{:keys [state ref]} path & args]
  "Like swap! but starts at the ref from enviroment. You can use the path to
   navigate into references, those will be resolved and the final target will
   receive the update event."
  (let [path (resolve-path @state (into ref path))]
    (if (and path (get-in @state path))
      (apply swap! state update-in path args)
      @state)))

(defn merge-entity [state x data & named-parameters]
  "Starting from a denormalized entity map, normalizes using class x.
   It assumes the entity is going to be normalized too, then get all
   normalized data and merge back into the app state and idents."
  (let [idents     (-> (fp/tree->db
                         (reify
                           fp/IQuery
                           (query [_] [{::root (fp/get-query x)}]))
                         {::root data} true)
                       (dissoc ::root ::fp/tables))
        root-ident (fp/ident x data)
        state      (merge-with (partial merge-with merge) state idents)]
    (if (seq named-parameters)
      (apply fp/integrate-ident state root-ident named-parameters)
      state)))

(defn initialized
  "Mark data as initialized so the value is not augmented via initial state."
  [data] (vary-meta data assoc ::initialized true))

(defn create-entity!
  "Create a new entity on the database for a given component. Example:

      (fp/defsc TodoItem
      [this {::keys []}]
      ; note the create-entity! data will be used as input for component initial state
      ; function to get the new entity initial state
      {:initial-state (fn [{::keys [title]}]
                        {::todo-id (random-uuid)
                         ::title   title})
       :ident         [::todo-id ::todo-id]
       :query         [::todo-id ::title]})

      (def todo-item (fp/factory TodoItem {:keyfn ::todo-id}))

      (fm/defmutation add-todo [todo]
        (action [env]
          ; append will integrate the new ident into the current ref (which is the todo
          ; list since it's the transaction reference)
          (db.h/create-entity! env TodoItem todo :append ::todo-items)))

      (fp/defsc TodoList
        [this {::keys [todo-items]}]
        {:initial-state (fn [_]
                          {})
         :ident         (fn [] [::todo-list \"singleton\"])
         :query         [{::todo-items (fp/get-query TodoItem)}]
         :css           []
         :css-include   []}
        (dom/div
          (dom/button {:onClick #(fp/transact! this [`(add-todo {::title \"Description\"})])})
          (mapv todo-item todo-items)))

  If you like to send the new entity data directly you can mark it as initialized:

      (let [todo (-> (fp/get-initial-state TodoItem todo)
                     (db.h/initialized))]
        (db.h/create-entity! env TodoItem todo :append ::todo-items))
  "
  [{:keys [state ref]} x data & named-parameters]
  (let [named-parameters (->> (partition 2 named-parameters)
                              (map (fn [[op path]] [op (conj ref path)]))
                              (apply concat))
        data'            (if (-> data meta ::initialized)
                           data
                           (fp/get-initial-state x data))
        data''           (if (empty? data') data data')]
    (apply swap! state merge-entity x data'' named-parameters)))

(defn- dissoc-in [m path]
  (cond-> m
    (get-in m (butlast path))
    (update-in (butlast path) dissoc (last path))))

(defn deep-remove-ref [state ref]
  "Remove a ref and all linked refs from it."
  (let [item   (get-in state ref)
        idents (into []
                     (comp (keep (fn [v]
                                   (cond
                                     (fulcro-ident? v)
                                     [v]

                                     (and (vector? v)
                                          (every? fulcro-ident? v))
                                     v)))
                           cat)
                     (vals item))]
    (reduce
      (fn [s i] (deep-remove-ref s i))
      (dissoc-in state ref)
      idents)))

(defn remove-edge! [{:keys [state ref]} field]
  "Remove edge data from a node. This will remove the ref and all associated data with it (recursive)."
  (let [children (get-in @state (conj ref field))]
    (cond
      (fulcro-ident? children)
      (swap! state (comp #(update-in % ref dissoc field)
                         #(deep-remove-ref % children)))

      (seq children)
      (swap! state (comp #(assoc-in % (conj ref field) [])
                         #(reduce deep-remove-ref % children))))))

(defn init-state
  "Starting from an ident and query, scan the DB initializing the components. This should be used to initialize data
  loaded from the network with fulcro fetch, this will recursively traverse using query information and merge the
  initial state with the current data (data load from the server takes priority)."
  ([state x ident]
   (let [initial  (fp/get-initial-state x nil)
         children (-> x fp/get-query fp/query->ast :children)
         data     (fp/db->tree (fp/get-query x) (get-in state ident) state)]
     (reduce
       (fn [s {:keys [type component key]}]
         (if (and (= :join type) component)
           (let [value (get-in state (conj ident key))]
             (cond
               (fulcro-ident? value)
               (init-state s component value)

               (vector? value)
               (reduce
                 (fn [s ident]
                   (if (fulcro-ident? ident)
                     (init-state s component ident)
                     s))
                 s
                 value)

               :else
               s))
           s))
       (merge-entity state x (merge initial data))
       children))))

(defn vec-remove-index [i v]
  "Remove an item from a vector via index."
  (->> (concat (subvec v 0 i)
               (subvec v (inc i) (count v)))
       (vec)))

(defn clean-keys
  "Set given keys to empty on current ref."
  [env keys]
  (let [empty-map (zipmap keys (repeat ""))]
    (swap-entity! env merge empty-map)))

(mutations/defmutation init-loaded-state [{:keys [ref component]}]
  (action [env]
    (let [{:keys [state]} env]
      (swap! state init-state component ref))))

(defn transform-remote [env ast]
  (let [ast (if (true? ast) (:ast env) ast)]
    (if-let [component (some-> ast :query meta :component)]
      (swap-entity! env assoc-in [::mutation-response :component] component))
    (-> ast
        (cond-> (:query ast) (update :query vary-meta dissoc :component))
        (mutations/with-target (conj (:ref env) ::mutation-response-swap)))))

(defn gen-pessimistic-mutation [sym arglist forms]
  (let [sym       sym
        ok-sym    (with-meta (symbol (str sym "-ok")) (meta sym))
        error-sym (with-meta (symbol (str sym "-error")) (meta sym))
        {[pre]     'pre-action
         [action]  'action
         [error]   'error-action
         [refresh] 'refresh
         remotes   nil} (group-by (fn [x] (#{'action 'error-action 'pre-action 'refresh} (first x))) forms)
        env       (gensym "env")
        refresh   (if refresh [refresh])
        remotes   (->> remotes
                       (mapv (fn [[s args & forms]]
                               (list s [env]
                                 `(let [~(first args) ~env]
                                    (transform-remote ~env (do ~@forms)))))))
        action    (or action '(action [_] nil))
        pre'      (some-> pre vec (assoc 0 'action) (->> (apply list)))
        initial   (if pre (into [pre'] remotes) remotes)]
    `(do
       (mutations/defmutation ~sym ~arglist ~@initial)
       (mutations/defmutation ~ok-sym ~arglist ~action ~@refresh)
       ~(if error `(mutations/defmutation ~error-sym ~arglist ~(-> error next (conj 'action)))))))

#?(:clj
   (defmacro defpmutation
     "Defines a pessimistic mutation. This is adapted to work with the pessimist mutation
     system for Shuffle. This works similar to the normal `defmutation`, but instead of
     doing the optimistic update right after the action, it delays to when the response
     is success from the remote. If the remote fails, the UI change is not going to be
     applied."
     [sym arglist & forms]
     (gen-pessimistic-mutation sym arglist forms)))

#?(:clj
   (s/fdef defpmutation
     :args (s/cat :sym symbol? :args vector? :forms (s/+ list?))))

(defn fetch-error
  "Get the error data for a given attribute."
  [this k]
  (get-in (fp/props this) [:com.wsscode.pathom.core/errors k]))

(defn mutation-response
  ([this]
   (if (fp/component? this)
     (mutation-response (-> this fp/get-reconciler fp/app-state deref) (fp/props this))
     (-> this ::mutation-response)))
  ([state props]
   (let [response (-> props ::mutation-response)]
     (if (fulcro.util/ident? response) (get-in state response) response))))

(defn mutation-loading? [this]
  (let [props (cond-> this (fp/component? this) fp/props)]
    (-> props ::mutation-response (fetch/loading?))))

(defn mutation-error?
  ([this]
   (-> (mutation-response this) (contains? :com.wsscode.pathom.core/mutation-errors)))
  ([state props]
   (-> (mutation-response state props) (contains? :com.wsscode.pathom.core/mutation-errors))))

(defn get-mutation [env k p]
  (if-let [m (get (methods mutations/mutate) k)]
    (m env k p)))

(defn call-mutation-action
  "Call a mutation action define in fulcro.client.mutations/mutate."
  [env k p]
  (if-let [h (-> (get-mutation env k p) :action)]
    (h)))

(s/def ::mutation-response (s/keys))

(mutations/defmutation start-pmutation [_]
  (action [env]
    (swap-entity! env assoc ::mutation-response {:fulcro.client.impl.data-fetch/type :loading})
    nil))

(mutations/defmutation mutation-network-error [{::keys [ref] :as p}]
  (action [env]
    (swap-entity! (assoc env :ref ref) assoc ::mutation-response
      (-> p
          (dissoc ::ref)
          (assoc ::fp/error "Network error")))
    nil))

(mutations/defmutation finish-pmutation [{:keys [ok-mutation error-mutation input]}]
  (action [env]
    (let [{:keys [state ref reconciler]} env
          {::keys [mutation-response mutation-response-swap] :as props} (get-in @state ref)]
      (if (mutation-error? @state (set/rename-keys props {::mutation-response-swap ::mutation-response}))
        (do
          (swap-entity! env assoc ::mutation-response mutation-response-swap)
          (call-mutation-action env error-mutation input))
        (do
          (if (:component mutation-response)
            (fp/merge-component! reconciler (:component mutation-response) mutation-response-swap))

          (if (mutation-loading? props) (swap-entity! env dissoc ::mutation-response))

          (call-mutation-action env ok-mutation input)))
      (swap-entity! env dissoc ::mutation-response-swap))))

(defn pmutate! [this mutation params]
  (let [ok-mutation    (symbol (str mutation "-ok"))
        error-mutation (symbol (str mutation "-error"))]
    (fp/ptransact! this `[(start-pmutation {})
                          ~(list mutation params)
                          (fulcro.client.data-fetch/fallback {:action mutation-network-error
                                                              ::ref   ~(fp/get-ident this)})
                          (finish-pmutation ~{:ok-mutation   ok-mutation
                                             :error-mutation error-mutation
                                             :input          params})])))

(mutations/defmutation multi-mutation
  "Creates a mutation that will execute a series of mutations. This can be useful to
  setup post-mutations that compose over other mutations.

  Example:

  (fetch/load app :root RootUI {:post-mutation `db.h/multi-mutation
                                :post-mutation-params {`some-mutation-a {:a :param}
                                                       `other-mutation  {:b :param}}})"
  [mutations]
  (action [env]
    (doseq [[sym params] mutations
            :when (symbol? sym)]
      ((:action (mutations/mutate env sym params))))))
