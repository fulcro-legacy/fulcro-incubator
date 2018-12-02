(ns fulcro.incubator.db-helpers
  #?(:cljs (:require-macros fulcro.incubator.db-helpers))
  (:require [clojure.spec.alpha :as s]
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

(defn swap-entity!
  "Swap/`update-in` then entity whose ident is `ref` in the given env."
  [{:keys [state ref] :as env} & args]
  (apply swap! state update-in ref args))

(defn resolve-path
  "Walks a db path, when find an ident it resets the path to that ident. Use to realize paths of relations."
  [state path]
  (loop [[h & t] path
         new-path []]
    (if h
      (let [np (conj new-path h)
            c  (get-in state np)]
        (if (fulcro-ident? c)
          (recur t c)
          (recur t (conj new-path h))))
      new-path)))

(defn swap-in!
  "Like swap! but starts at the ref from `env`, adds in supplied `path` elements (resolving across idents if necessary).
   Finally runs an update-in on that resultant path with the given `args`.

   Roughly equivalent to:

   ```
   (swap! (:state env) update-in (resolve-path @state (into (:ref env) path)) args)
   ```

   with a small bit of additional sanity checking.
   "
  [{:keys [state ref]} path & args]
  (let [path (resolve-path @state (into ref path))]
    (if (and path (get-in @state path))
      (apply swap! state update-in path args)
      @state)))

(defn merge-entity*
  "Merge the data-tree using the query from component-class, then run integrate-ident with the names parameters."
  [state-map component-class data-tree & integrate-ident-named-parameters]
  (let [idents     (-> (fp/tree->db [{::root (fp/get-query component-class)}] {::root data-tree} true)
                     (dissoc ::root ::fp/tables))
        root-ident (fp/ident component-class data-tree)
        state      (merge-with (partial merge-with merge) state-map idents)]
    (if (seq integrate-ident-named-parameters)
      (apply mutations/integrate-ident* state root-ident integrate-ident-named-parameters)
      state)))

(def merge-entity "DEPRECATED. Use merge-entity*" merge-entity*)

(defn initialized
  "Mark `data` so that the value is not augmented via get-initial-state merging when using create-entity* and create-entity!"
  [data] (vary-meta data assoc ::initialized true))

(defn create-entity*
  "Mutation helper.  Create a new instance of the given component class. This differs from Fulcro's standard
  merge-component because it combines the `data` with the component's initial state.

  The named-parameters are passed to Fulcro integrate-ident* to add the ident of the new entity into other parts
  of your app state. NOTE: The create-entity! function sets these to the invoking components ref + a field. This function
  does *not*.

  See create-entity!"
  [state-map component-class data & named-parameters]
  (let [data'  (if (-> data meta ::initialized)
                 data
                 (fp/get-initial-state component-class data))
        data'' (if (empty? data') data data')]
    (apply merge-entity* state-map component-class data'' named-parameters)))

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
  [{:keys [state ref]} component-class data & named-parameters]
  (let [named-parameters (->> (partition 2 named-parameters)
                           (map (fn [[op path]] [op (conj ref path)]))
                           (apply concat))]
    (apply swap! state create-entity* component-class data ref named-parameters)))

(defn- dissoc-in
  "Remove the given leaf of the `path` from recursive data structure `m`"
  [m path]
  (cond-> m
    (get-in m (butlast path))
    (update-in (butlast path) dissoc (last path))))

(defn deep-remove-entity*
  "Recursively remove a table entry (by ident) and anything it recursively points to."
  [state-map ident]
  (let [item   (get-in state-map ident)
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
      (fn [s i] (deep-remove-entity* s i))
      (dissoc-in state-map ident)
      idents)))

(def deep-remove-ref "DEPRECATED: Use deep-remote-entity*" deep-remove-entity*)

(defn remove-edge!
  "Given a mutation env and a field: Remove the graph edge designated by `field`, and recursively remove the data
   for the referenced entity/ies (to-one or to-many)."
  [{:keys [state ref] :as env} field]
  (let [children (get-in @state (conj ref field))]
    (cond
      (fulcro-ident? children)
      (swap! state (comp #(update-in % ref dissoc field)
                     #(deep-remove-entity* % children)))

      (seq children)
      (swap! state (comp #(assoc-in % (conj ref field) [])
                     #(reduce deep-remove-entity* % children))))))

(defn init-loaded-state*
  "Fill in any declared component initial state over a just-loaded entity.  This recursively follows the query of the
  component, calling `get-initial-state` at each level, and fills in any *missing* fields with the data from initial
  state.  This allows a newly-fetched item to have initial state for ui-only attributes."
  ([state component-class ident]
   (let [initial  (fp/get-initial-state component-class nil)
         children (-> component-class fp/get-query fp/query->ast :children)
         data     (fp/db->tree (fp/get-query component-class) (get-in state ident) state)]
     (reduce
       (fn [s {:keys [type component key]}]
         (if (and (= :join type) component)
           (let [value (get-in state (conj ident key))]
             (cond
               (fulcro-ident? value)
               (init-loaded-state* s component value)

               (vector? value)
               (reduce
                 (fn [s ident]
                   (if (fulcro-ident? ident)
                     (init-loaded-state* s component ident)
                     s))
                 s
                 value)

               :else
               s))
           s))
       (merge-entity* state component-class (merge initial data))
       children))))

(def init-state "DEPRECATED: use init-loaded-state*" init-loaded-state*)

(defn vec-remove-index
  "Remove an item from a vector via index."
  [i v]
  (->> (concat (subvec v 0 i)
         (subvec v (inc i) (count v)))
    (vec)))

(defn clean-keys
  "Set given keys to empty strings on current ref of `env`."
  [env keys]
  (let [empty-map (zipmap keys (repeat ""))]
    (swap-entity! env merge empty-map)))

(mutations/defmutation init-loaded-state
  "Mutation: fills in just-loaded components with data from `get-initial-state`, *without* overwriting anything
  that is already there. This is useful to add defaults for things like :ui/... attributes on your components that
  just came in from a server and had no initial UI state.  See `init-state` for a version you can use in your
  mutations."
  [{:keys [ref component]}]
  (action [env]
    (let [{:keys [state]} env]
      (swap! state init-loaded-state* component ref))))

(mutations/defmutation multi-mutation
  "Creates a mutation that will execute a series of mutations. This can be useful to
  setup post-mutations on load that compose other mutations.

  Example:

  (fetch/load app :root RootUI {:post-mutation `db.h/multi-mutation
                                :post-mutation-params {`some-mutation-a {:a :param}
                                                       `other-mutation  {:b :param}}})"
  [mutations]
  (action [env]
    (doseq [[sym params] mutations
            :when (symbol? sym)]
      ((:action (mutations/mutate env sym params))))))

;; ================================================================================
;; NOTE: THE FUNCTIONS BELOW ARE DEPRECATED. Migrate to the pessimistic-mutations namespace instead.
;; ================================================================================

;; a safe way to save component in app state
(defn- response-component [component] (with-meta {} {:component component}))
(defn- get-response-component [response] (-> response :component meta :component))

(defn- transform-remote
  "A helper function that you should not use. "
  [env ast]
  (let [ast (if (true? ast) (:ast env) ast)]
    (if-let [component (some-> ast :query meta :component)]
      (swap-entity! env assoc-in [::mutation-response :component] (response-component component)))
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
  "Is the mutation in error. This version assumes you use :com.wsscode.pathom.core/mutation-errors as a key in your
  response to indicate an error."
  ([this]
   (-> (mutation-response this) (contains? :com.wsscode.pathom.core/mutation-errors)))
  ([state props]
   (-> (mutation-response state props) (contains? :com.wsscode.pathom.core/mutation-errors))))

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
  [{::keys [ref] :as p}]
  (action [env]
    (swap-entity! (assoc env :ref ref) assoc ::mutation-response
      (-> p
        (dissoc ::ref)
        (assoc ::fp/error "Network error")))
    nil))

(mutations/defmutation start-pmutation
  "INTERNAL USE mutation."
  [_]
  (action [env]
    (swap-entity! env assoc ::mutation-response {:fulcro.client.impl.data-fetch/type :loading})
    nil))

(mutations/defmutation finish-pmutation
  "INTERNAL USE mutation."
  [{:keys [ok-mutation error-mutation input]}]
  (action [env]
    (let [{:keys [state ref reconciler]} env
          {::keys [mutation-response mutation-response-swap] :as props} (get-in @state ref)]
      (if (mutation-error? @state (set/rename-keys props {::mutation-response-swap ::mutation-response}))
        (do
          (swap-entity! env assoc ::mutation-response mutation-response-swap)
          (call-mutation-action env error-mutation input))
        (do
          (when-let [component (get-response-component mutation-response)]
            (fp/merge-component! reconciler component mutation-response-swap))

          (when (mutation-loading? props) (swap-entity! env dissoc ::mutation-response))

          (call-mutation-action env ok-mutation input)))
      (swap-entity! env dissoc ::mutation-response-swap))))

(defn pmutate!
  "Run a pmutation defined by `defpmutation`.

  this - The component whose ident will be used for status reporting on the progress of the mutation.
  mutation - The symbol of the mutation you want to run.
  params - The parameter map for the mutation.

  The `params` map can contain the additional special keys which are used to augment the functionality:

  `::pm/error-marker` - An opaque value that will be placed in the mutation response if there is an error. You can
  use this to distinguish the error when components in the UI share idents.

  "
  [this mutation params]
  (let [ok-mutation    (symbol (str mutation "-ok"))
        error-mutation (symbol (str mutation "-error"))]
    (fp/ptransact! this `[(start-pmutation {})
                          ~(list mutation params)
                          (fulcro.client.data-fetch/fallback {:action mutation-network-error
                                                              ::ref   ~(fp/get-ident this)})
                          (finish-pmutation ~{:ok-mutation    ok-mutation
                                              :error-mutation error-mutation
                                              :input          params})])))
