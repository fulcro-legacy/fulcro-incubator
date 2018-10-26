(ns fulcro.incubator.io-progress
  (:require [fulcro.client.primitives :as prim]
            [fulcro.client.data-fetch :as df]
            [fulcro.client.mutations :refer [defmutation]]
            [fulcro.incubator.pessimistic-mutations :as pm]))

(defn update-loading-visible!
  "A helper to use with components to do flicker-free activity indicators in the UI:

  ```
   :componentDidUpdate (fn [pp ps] (util/update-loading-visible this pp {:timeout 200}))
  ```

  which will set the component-local state :loading-visible? to true/false based on the pmutate
  mutation-response and load marker status, but only if the given timeout has elapsed, preventing flicker.

  `this` - The component that has a targeted load or mutate.
  `prior-props` - The prior props (from the args passed to componentDidUpdate).
  `options` is an optional map that can include:
    - `:timeout` - The amount of time to wait before setting loading-visible? to true. Defaults to 100ms.
    - `:marker` - The name of the load marker to look for. Defaults to the ident of `this`.
    - `:load-key` - The name to use in component local state instead of the default `:loading-visible?`.
  "
  ([this prior-props] (update-loading-visible! this prior-props {}))
  ([this prior-props options]
    #?(:cljs
       (let [{:keys [timeout marker load-key]} options
             load-marker        (or marker (prim/get-ident this))
             state-key          (or load-key :loading-visible?)
             timeout            (or timeout 100)
             {old-response ::pm/mutation-response} prior-props
             {::pm/keys [mutation-response] :as props} (prim/props this)
             old-status         (::pm/status old-response)
             old-loading?       (df/loading? (get-in prior-props [df/marker-table load-marker]))
             new-loading?       (df/loading? (get-in props [df/marker-table load-marker]))
             new-status         (::pm/status mutation-response)
             currently-loading? (or new-loading? (= :loading new-status))
             v                  (prim/get-state this state-key)]
         (if (and currently-loading? (or (not= old-loading? new-loading?) (not= old-status new-status)))
           (js/setTimeout (fn []
                            (let [props  (prim/props this)
                                  status (or
                                           (boolean (get-in props [df/marker-table (prim/get-ident this)]))
                                           (= :loading (get-in props [::pm/mutation-response ::pm/status])))]
                              (prim/set-state! this {state-key status}))) timeout)
           (when (and v (not currently-loading?))
             (prim/set-state! this {state-key false})))))))

(defn load-error
  "Returns the data related to a load error.

  Load marker tracking requires that you have queried for `[df/marker-table '_]` in your component's query, and
  that you're using the component's ident as the marker name.

  `this` - The component to check for errors.
  `options` - An optional map:
     - `:marker` The name of the load marker, if not the component's ident.

  Returns false if there is no problem; otherwise returns the data-fetch marker.
  "
  ([this]
   (load-error this {}))
  ([this options]
   (let [load-marker  (or (:marker options) (prim/get-ident this))
         props        (prim/props this)
         fetch-marker (get-in props [df/marker-table load-marker])
         load-error?  (df/failed? fetch-marker)]
     (and load-error? fetch-marker))))

(defn mutation-error
  "Returns the data related to a load/mutation error if there is a pessimistic mutation error or load marker error in props.

  This will be the value of `::pm/mutation-errors` if present, otherwise it will be the entire `::pm/mutation-response`.

  Mutation response error tracking requires that you query for `::pm/mutation-response` in your component's query; otherwise
  the error is only visible to the `error-action` section of your mutation.

  Load marker tracking requires that you have queried for `[df/marker-table '_]` in your component's query, and
  that you're using the component's ident as the marker name.

  `this` - The component to check for errors.

  Returns false if there is no problem; otherwise returns the value of ::pm/mutation-response.
  "
  [this]
  (let [props           (prim/props this)
        mvalue          (some-> props ::pm/mutation-response)
        mutation-error? (contains? pm/error-states (some-> mvalue ::pm/status))
        result          (or (::pm/mutation-errors mvalue) mvalue)]
    (and mutation-error? result)))

(defn io-error
  "Returns the data related to a load/mutation error if there is a pessimistic mutation error or load marker error in props.

  Mutation response error tracking requires that you query for `::pm/mutation-response` in your component's query; otherwise
  the error is only visible to the `error-action` section of your mutation.

  Load marker tracking requires that you have queried for `[df/marker-table '_]` in your component's query, and
  that you're using the component's ident as the marker name.

  `this` - The component to check for errors.
  `options` - An optional map:
     - `:marker` The name of the load marker, if not the component's ident.

  Returns false if there is no problem; otherwise returns the value of ::pm/mutation-response if it is a mutation error,
  or the data-fetch marker if it is a failed load.
  "
  ([this]
   (io-error this {}))
  ([this options]
   (or (mutation-error this) (load-error this options))))

(defn clear-errors*
  "Mutation helper variant of clear-errors.

  Options is a map that *must* include the ident of the component, and may optionally include the
  custom marker name of the load marker (if it isn't the ident)."
  [state-map {:keys [ident marker]}]
  (let [load-marker (or marker ident)]
    (-> state-map
      (update df/marker-table dissoc load-marker)
      (update-in ident dissoc ::pm/mutation-response)))
  )
(defmutation clear-errors
  "Mutation: Clear the errors on a component.  Defaults to `this` of the mutation (using ref), but can be passed a literal
  :ident, and an optional :marker parameter (to find the component and/or load marker). Both default to the ident of
  the component that runs this mutation, so no parameters are required."
  [{:keys [ident marker]}]
  (action [{:keys [ref state]}]
    (let [load-marker (or marker ref)
          ref         (or ident ref)]
      (swap! state clear-errors* {:ident ref :marker load-marker}))))

(defn busy?
  "UI Helper. Returns true if the given component has a targeted load or pmutate actively running on it.  To work with loads
  you MUST set `:marker (get-ident this)` in your call to `load`."
  [this]
  #?(:cljs
     (let [{::pm/keys [mutation-response] :as props} (prim/props this)
           new-loading? (boolean (get-in props [df/marker-table (prim/get-ident this)]))
           new-status   (::pm/status mutation-response)]
       (or new-loading? (= :loading new-status)))))
