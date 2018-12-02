(ns fulcro.incubator.dynamic-routing-macro-test
  (:require [clojure.test :refer :all]
            [fulcro-spec.core :refer [assertions]]
            [fulcro.incubator.dynamic-routing :as dr]
            [fulcro.incubator.ui-state-machines :as uism]
            [fulcro.client.primitives :as prim]))

#_(deftest defrouter-macro-tests
  (let [actual       (dr/defrouter* {} 'ARouter {:router-targets #{'A 'B}})
        [nm sym args options body] actual
        {:keys [query ident protocols initial-state]} options
        generated-id (-> ident (nth 2) second)]
    (assertions
      "the generated ID is a keyword"
      (keyword? generated-id) => true
      "emits a defsc"
      nm => `prim/defsc
      "with an args list that destructures the correct params"
      args => ['this {:as 'props ::dr/keys ['id 'current-route]}]
      "has the correct initial query"
      query => `[::dr/id
                 [::uism/asm-id ~generated-id]
                 {::dr/current-route (prim/get-query ~'A)}
                 {:alt0 (prim/get-query ~'B)}]
      "has the corrent ident"
      ident => `(fn [] [::dr/id ~generated-id])
      "has a protocol declaration for getting the targets"
      protocols => `[~'static dr/Router (~'get-targets [~'c] #{~'A ~'B})]
      "has an initial state that will cause all routes to initialize"
      initial-state => `(fn [~'params]
                          {::dr/id            ~generated-id
                           ::dr/current-route (prim/get-initial-state ~'A ~'params)
                           :alt0              (prim/get-initial-state ~'B {})})
      "has a body that will render the current route"
      body => `(let [~'current-state (get-in ~'props [[::uism/asm-id ~generated-id] ::uism/active-state])]
                 (case ~'current-state
                   (:routed :deferred :initial :pending :failed)
                   (if-let [~'class (fulcro.incubator.dynamic-routing/current-route-class ~'this)]
                     (let [~'factory (prim/factory ~'class)]
                       (~'factory ~'current-route)))
                   nil))))
  (let [actual       (dr/defrouter* {} 'ARouter {:router-targets #{'A 'B} :initial-ui '(dom/div nil "TODO")})
        [_ _ _ options body] actual
        {:keys [ident]} options
        generated-id (-> ident (nth 2) second)]
    (assertions
      "has a body that will render the current route"
      body => `(let [~'current-state (get-in ~'props [[::uism/asm-id ~generated-id] ::uism/active-state])]
                 (case ~'current-state
                   (:routed :deferred :pending :failed)
                   (if-let [~'class (fulcro.incubator.dynamic-routing/current-route-class ~'this)]
                     (let [~'factory (prim/factory ~'class)]
                       (~'factory ~'current-route)))
                   :initial (~'dom/div nil "TODO")
                   (~'dom/div nil "TODO"))))))
