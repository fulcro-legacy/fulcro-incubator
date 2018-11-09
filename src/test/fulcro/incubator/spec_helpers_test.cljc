(ns fulcro.incubator.spec-helpers-test
  (:require
    [clojure.test :as t :refer [deftest testing is]]
    [fulcro.incubator.spec-helpers :as g :refer [=> | <- >defn >defn- >fdef ?]]))

(deftest generates-the-correct-spec-and-public-function
  (let [actual           (macroexpand-1 '(fulcro.incubator.spec-helpers/>defn f [x]
                                           [pos-int? => pos-int?]
                                           (inc x)))
        expected #?(:clj '(do
                            (clojure.spec.alpha/fdef f
                              :args (clojure.spec.alpha/cat :x pos-int?)
                              :ret pos-int?)
                            (defn f
                              {:jsdoc ["@type {function(!number): !number}"]}
                              [x]
                              (inc x)))
                    :cljs '(do
                             (cljs.spec.alpha/fdef f
                               :args (cljs.spec.alpha/cat :x pos-int?)
                               :ret pos-int?)
                             (defn f
                               {:jsdoc ["@type {function(!number): !number}"]}
                               [x]
                               (inc x))))]
    (is (= actual expected))))

(deftest generates-the-correct-spec-and-private-function
  (let [actual           (macroexpand-1 '(fulcro.incubator.spec-helpers/>defn- f [x]
                                           [pos-int? => pos-int?]
                                           (inc x)))
        expected #?(:clj '(do
                            (clojure.spec.alpha/fdef f
                              :args (clojure.spec.alpha/cat :x pos-int?)
                              :ret pos-int?)
                            (defn- f
                              {:jsdoc ["@type {function(!number): !number}"]}
                              [x]
                              (inc x)))
                    :cljs '(do
                             (cljs.spec.alpha/fdef f
                               :args (cljs.spec.alpha/cat :x pos-int?)
                               :ret pos-int?)
                             (defn- f
                               {:jsdoc ["@type {function(!number): !number}"]}
                               [x]
                               (inc x))))]
    (is (= actual expected))))
