(ns fulcro.incubator.mutation-interface-ws
  (:require [nubank.workspaces.core :refer [deftest]]
            [fulcro-spec.core :refer [specification behavior component assertions when-mocking]]
            [cljs.test :refer [is testing]]
            [fulcro.incubator.mutation-interface :as mi :refer [declare-mutation]]
            [fulcro.client.primitives :as fp]
            [clojure.walk :as walk]
            [fulcro.client.mutations :as fm]
            [cljs.spec.alpha :as s]))

(s/def ::a string?)
(s/def ::b pos-int?)

(mi/declare-mutation boo
  "Mutation to do blah"
  'a.b/c
  (s/keys :req-un [::a ::b]))

(mi/declare-mutation foo
  'a.b/d
  map?)

(deftest declaring-mutations
  (assertions
    "can be used to generate a mutation expression without quoting"
    (boo {:a "hello"}) => (list 'a.b/c {:a "hello"})
    "doc string is optional"
    (foo {}) => (list 'a.b/d {})
    "Can be called without arguments"
    (foo) => (list 'a.b/d {})
    "ignores specs without dynamic binding of *checked-mutations*"
    (boo {:a 22 :b "hello"}) => (list 'a.b/c {:a 22 :b "hello"})
    "enforces specs with dynamic binding"
    (binding [mi/*checked-mutations* true]
      (boo {:a 22 :b "hello"})) =throws=> {:regex #"Mutation failed spec"}))
