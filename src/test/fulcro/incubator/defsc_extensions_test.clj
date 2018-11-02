(ns fulcro.incubator.defsc-extensions-test
  (:require [clojure.test :refer :all]
            [fulcro.incubator.defsc-extensions :as dext]
            [fulcro.client.primitives :refer [defsc]]
            [fulcro-spec.core :refer [assertions]]))

(defprotocol MyThing
  (method-1 [this] "do 1")
  (method-2 [this] "do 2"))

(defprotocol Other
  (do-it [this a b]))

(defprotocol Unsupported
  (method [this] [this arg]))

(deftest morph-option-to-method-test
  (testing "Morphs the `fn` (without this) to a protocol method name with the proper defsc declaration of `this`"
    (let [env    {}
          actual (dext/morph-option-to-method env 'this-sym 'method-1 '[this] '(fn [] (boo 1)))]
      (is (= '(method-1 [this-sym] (boo 1)) actual))))
  (testing "Supports a fn with higher arity"
    (let [env    {}
          actual (dext/morph-option-to-method env 'this-sym 'method-1 '[this a b] '(fn [x y] (boo x y)))]
      (is (= '(method-1 [this-sym x y] (boo x y)) actual)))))

(deftest morph-options-to-methods-test
  (let [env                 {}
        this                'this-sym
        methods             (:sigs MyThing)
        unsupported-methods (:sigs Unsupported)]
    (assertions
      "Reports an error if the user fails to provide one of the protocol methods"
      (dext/morph-options-to-methods env this methods {:method-1 '(fn [] (boo 1))}) =throws=> {:regex #"required.*:method-2"}
      "Reports an error if the protocol has multi-arity protocol methods"
      (dext/morph-options-to-methods env this unsupported-methods {:method '(fn [] (boo 1))}) =throws=> {:regex #"Extended.*does not.*multi-arity"}
      "Correctly morphs the options to methods"
      (dext/morph-options-to-methods env this methods {:method-1 '(fn [] (boo 1))
                                                       :method-2 '(fn [] (foo 2))}) => '[(method-1 [this-sym] (boo 1)) (method-2 [this-sym] (foo 2))])))

(deftest resolve*-test
  (assertions
    "resolve* reports an error if it cannot resolve the symbol"
    (dext/resolve* {} {} `undefined) =throws=> {:regex #"Cannot resolve.*undefined"}
    "resolve* gives back the real item when it exists"
    (dext/resolve* {} {} `MyThing) => MyThing))

(deftest emit-protocols-test
  (assertions
    "emits the correct forms for embedding a static protocol in defsc :protocols option"
    (dext/emit-protocol {} 't `MyThing {:method-1 '(fn [] (boo 1))
                                        :method-2 '(fn [] (foo 2))} true) => '[static fulcro.incubator.defsc-extensions-test/MyThing (method-1 [t] (boo 1)) (method-2 [t] (foo 2))]
    "emits the correct forms for embedding a non-static protocol in defsc :protocols option"
    (dext/emit-protocol {} 't `MyThing {:method-1 '(fn [] (boo 1))
                                        :method-2 '(fn [] (foo 2))} false) => '[fulcro.incubator.defsc-extensions-test/MyThing (method-1 [t] (boo 1)) (method-2 [t] (foo 2))]))

(deftest defsc-options->protocols-test
  (assertions
    "Emits the correct protocol vector for a single non-static protocol"
    (dext/defsc-options->protocols {}
      'th '{:method-1 (fn [] true)
            :method-2 (fn [] false)}
      [[`MyThing false]])
    => '[fulcro.incubator.defsc-extensions-test/MyThing (method-1 [th] true) (method-2 [th] false)]
    "Emits the correct protocol vector for a single static protocol"
    (dext/defsc-options->protocols {}
      'th '{:method-1 (fn [] true)
            :method-2 (fn [] false)}
      [[`MyThing true]])
    => '[static fulcro.incubator.defsc-extensions-test/MyThing (method-1 [th] true) (method-2 [th] false)]
    "Emits the correct protocol vector for a multiple protocols"
    (dext/defsc-options->protocols {}
      'th '{:method-1 (fn [] true)
            :do-it    (fn [x y] 42)
            :method-2 (fn [] false)}
      [[`MyThing true] [`Other false]])
    => '[static fulcro.incubator.defsc-extensions-test/MyThing (method-1 [th] true) (method-2 [th] false)
         fulcro.incubator.defsc-extensions-test/Other (do-it [th x y] 42)]))

(deftest strip-protocol-options-test
  (assertions
    "Removes the option keys from the options map that correspond to the protocol methods"
    (dext/strip-protocol-options {} '{:method-1 (fn [] true) :method-2 (fn [] false)} [[`MyThing true]])
    => '{}
    "Leaves other options alone"
    (dext/strip-protocol-options {} '{:ident ...} [[`MyThing true]])
    => '{:ident ...}))


(deftest rewrite-defsc-protocols-test
  (assertions
    "Can rewrite option methods into protocols"
    (dext/rewrite-defsc-protocols {}
      '(Component [this args]
         {:do-it (fn [m n] 55)
          :ident [:table :id]}
         (js/console.log "Hello")
         (dom/div "TODO"))
      [[`Other false]])
    => '(fulcro.client.primitives/defsc Component [this args]
          {:ident     [:table :id]
           :protocols [fulcro.incubator.defsc-extensions-test/Other
                       (do-it [this m n] 55)]}
          (js/console.log "Hello")
          (dom/div "TODO"))

    "Accepts doc string"
    (dext/rewrite-defsc-protocols {}
      '(Component
         "My Component"
         [this args]
         {:do-it (fn [m n] 55)
          :ident [:table :id]}
         (js/console.log "Hello")
         (dom/div "TODO"))
      [[`Other false]])
    => '(fulcro.client.primitives/defsc Component
          "My Component"
          [this args]
          {:ident     [:table :id]
           :protocols [fulcro.incubator.defsc-extensions-test/Other
                       (do-it [this m n] 55)]}
          (js/console.log "Hello")
          (dom/div "TODO"))

    "Can be used with no protocols"
    (dext/rewrite-defsc-protocols {}
      '(Component [this args]
         {:ident [:table :id]}
         (dom/div "TODO"))
      [])
    => '(fulcro.client.primitives/defsc Component [this args]
          {:ident [:table :id]}
          (dom/div "TODO"))))

(deftest defextended-defsc-test
  (is (= '(clojure.core/defmacro
            defsc-router
            [& forms]
            (fulcro.incubator.defsc-extensions/rewrite-defsc-protocols
              &env
              forms
              [[fulcro.incubator.defsc-extensions-test/Other true]]))
        (macroexpand-1 `(dext/defextended-defsc ~'defsc-router [[Other true]])))))


