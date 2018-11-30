(ns fulcro.incubator.dynamic-routing-macro-test
  (:require [clojure.test :refer :all]
            [fulcro-spec.core :refer [assertions]]
            [fulcro.incubator.dynamic-routing :as dr]
            [fulcro.client.primitives :as prim]))

(deftest defrouter-macro-tests
  (let [actual       (dr/defrouter* {} 'ARouter ['A 'B])
        [nm sym args options body] actual
        {:keys [query ident protocols initial-state]} options
        generated-id (-> ident (nth 2) second)]
    (assertions
      "the generated ID is a string"
      (string? generated-id) => true
      "emits a defsc"
      nm => `prim/defsc
      "with an args list that destructures the correct params"
      args => ['this {:as 'props ::dr/keys ['id 'current-route]}]
      "has the correct initial query"
      query => `[::dr/id
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
      body => `(if-let [~'class (fulcro.incubator.dynamic-routing/current-route-class ~'this)]
                 (let [~'factory (prim/factory ~'class)]
                   (~'factory ~'current-route))))))
