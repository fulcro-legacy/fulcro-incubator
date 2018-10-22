(ns fulcro.incubator.mutation-interface-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [fulcro-spec.core :refer [assertions]]
            [fulcro.incubator.mutation-interface :as mi :refer [declare-mutation]]))

(s/def ::a string?)
(s/def ::b pos-int?)

(mi/declare-mutation boo
  "Mutation to do blah"
  'a.b/c
  (s/keys :req-un [::a ::b]))

(mi/declare-mutation foo
  'a.b/d
  map?)

(mi/declare-mutation bar 'x/bar)

(deftest declaring-mutations
  (assertions
    "Default to allowing any params when checked."
    (mi/! (bar {:a 22})) => (list 'x/bar {:a 22})
    (mi/! (bar)) => (list 'x/bar {})
    (mi/! (bar {})) => (list 'x/bar {})
    "can be used to generate a mutation expression without quoting"
    (boo {:a "hello"}) => (list 'a.b/c {:a "hello"})
    "doc string is optional"
    (foo {}) => (list 'a.b/d {})
    "Can be called without arguments"
    (foo) => (list 'a.b/d {})
    "ignores specs without dynamic binding of *checked-mutations*"
    (boo {:a 22 :b "hello"}) => (list 'a.b/c {:a 22 :b "hello"})
    "enforces specs with dynamic binding"
    (mi/! (boo {:a 22 :b "hello"})) =throws=> {:regex #"Mutation failed spec"}
    "can be distinguished as the interface"
    (mi/mutation-declaration? foo) => true
    (mi/mutation-declaration? (foo)) => false
    (mi/mutation-declaration? 'a.b/c) => false))
