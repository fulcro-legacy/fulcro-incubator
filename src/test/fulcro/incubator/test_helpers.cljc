(ns fulcro.incubator.test-helpers
  (:require
    #?(:cljs ["enzyme" :refer [shallow instance]])
    #?(:clj [fulcro.client.dom-server :as dom])
    [fulcro.client.primitives :as prim :refer [defsc]]))

(defn mock-component
  "Returns a shallow-rendered react Component (rendered instance) that prim/component? will return true for."
  [fulcro-class props]
  #?(:clj  ((prim/factory fulcro-class) props)
     :cljs (.instance (shallow ((prim/factory fulcro-class) props)))))
