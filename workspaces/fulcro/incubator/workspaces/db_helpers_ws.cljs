(ns fulcro.incubator.workspaces.db-helpers-ws
  (:require [nubank.workspaces.core :refer [deftest]]
            [fulcro-spec.core :refer [specification behavior component assertions when-mocking]]
            [cljs.test :refer [is testing]]
            [fulcro.incubator.db-helpers :as db.h]
            [fulcro.client.primitives :as fp]
            [clojure.walk :as walk]
            [fulcro.client.mutations :as fm]))

(fp/defsc PureUI
  [this {::keys []} computed]
  {:initial-state {:ui/id    "id"
                   :ui/value :bar}
   :ident         [:ui/id :ui/id]
   :query         [:ui/id :ui/value]})

(fp/defsc ManySomething
  [this {::keys []} computed]
  {:initial-state {:something/id (random-uuid)
                   :ui/oh-many   true}
   :ident         [:something/id :something/id]
   :query         [:something/id :ui/oh-many]})

(fp/defsc CustomerSomething
  [this {::keys []} computed]
  {:initial-state {:something/id (random-uuid)
                   :ui/expanded? false
                   :ui/pure      {}}
   :ident         [:something/id :something/id]
   :query         [:something/id :ui/expanded?
                   {:ui/pure (fp/get-query PureUI)}]})

(fp/defsc Customer
  [this {::keys []} computed]
  {:initial-state {:ui/bla "meh"}
   :ident         [:customer/id :customer/id]
   :query         [:customer/id :ui/bla
                   {:something (fp/get-query CustomerSomething)}
                   {:many (fp/get-query ManySomething)}]})

(fp/defsc CustomerDeep
  [this {::keys []} computed]
  {:initial-state {}
   :ident         [:customer/id :customer/id]
   :query         [:customer/id
                   {:go [{:something (fp/get-query CustomerSomething)}]}]})

(deftest swap-entity!
  (behavior "Swap from env"
    (assertions
      "Works"
      (let [env {:state (atom {}) :ref [:customer/id 123]}]
        (db.h/swap-entity! env assoc :ui/data "foo"))
      => {:customer/id {123 {:ui/data "foo"}}})))

(deftest resolve-path
  (assertions
    "Keep the pointer on start"
    (db.h/resolve-path
      {:item/id {123 {:foo "bar"}}
       :ui/root [:item/id 123]}
      [:ui/root])
    => [:item/id 123]

    "Get attribute from linked reference"
    (db.h/resolve-path
      {:item/id {123 {:foo "bar"}}
       :ui/root [:item/id 123]}
      [:ui/root :foo])
    => [:item/id 123 :foo]

    "Navigate deep"
    (db.h/resolve-path
      {:item/id {123 {:rel [:item/id 222]}
                 222 {:foo "bar"}}
       :ui/root [:item/id 123]}
      [:ui/root :rel :foo])
    => [:item/id 222 :foo]))

(deftest init-state
  (assertions
    "Works on simple cases"
    (db.h/init-state {:customer/id {123 {:customer/id 123}}} Customer [:customer/id 123])
    => {:customer/id {123 {:customer/id 123
                           :ui/bla      "meh"}}}

    "Initialize to-one relations, and auto initialize ui joins"
    (db.h/init-state {:customer/id  {333 {:customer/id 333
                                          :something   [:something/id 111]}}
                      :something/id {111 {:something/id 111}}} Customer [:customer/id 333])
    => {:customer/id  {333 {:customer/id 333
                            :something   [:something/id 111]
                            :ui/bla      "meh"}}
        :something/id {111 {:something/id 111
                            :ui/expanded? false
                            :ui/pure      [:ui/id "id"]}}
        :ui/id        {"id" {:ui/id    "id"
                             :ui/value :bar}}}

    "Initialize to-many relations"
    (db.h/init-state {:customer/id  {333 {:customer/id 333
                                          :many        [[:something/id 111]
                                                        [:something/id 222]]}}
                      :something/id {111 {:something/id 111}
                                     222 {:something/id 222}}}
      Customer [:customer/id 333])
    => {:customer/id  {333 {:customer/id 333
                            :many        [[:something/id 111]
                                          [:something/id 222]]
                            :ui/bla      "meh"}}
        :something/id {111 {:something/id 111
                            :ui/oh-many   true}
                       222 {:something/id 222
                            :ui/oh-many   true}}}))

(deftest swap-in!
  (assertions
    (let [env {:state (atom {:customer/id {123 {:>/child [:foo 321]}}
                             :foo         {321 {:bar [:baz 42]}}
                             :baz         {42 {:x 1}}})
               :ref   [:customer/id 123]}]
      (db.h/swap-in! env [:>/child :bar] assoc :y 2))
    => {:customer/id {123 {:>/child [:foo 321]}}
        :foo         {321 {:bar [:baz 42]}}
        :baz         {42 {:x 1 :y 2}}}))

(defn expand-meta [f]
  (walk/postwalk
    (fn [x]
      ; calls have metadata with line/column numbers in them in clj...ignore those
      (if (and (meta x) (not= #{:line :column} (-> x meta keys set)))
        {::source x ::meta (meta x)}
        x))
    f))

(deftest test-transform-remote
  (assertions
    "add target to ast"
    (-> (db.h/transform-remote {:ref [:item 123]} {:key 'my-mutation}) (expand-meta))
    => '{:dispatch-key my-mutation
         :key          my-mutation
         :params       {}
         :type         :call
         :query        {::source [*]
                        ::meta   {:fulcro.client.impl.data-fetch/target
                                  [:item
                                   123
                                   ::db.h/mutation-response-swap]}}
         :children     [{:dispatch-key * :key *}]}

    "add target to ast true value"
    (-> (db.h/transform-remote {:ref [:item 123] :ast {:key 'my-mutation}} true)
        (expand-meta))
    => '{:dispatch-key my-mutation
         :key          my-mutation
         :params       {}
         :type         :call
         :query        {::source [*]
                        ::meta   {:fulcro.client.impl.data-fetch/target
                                  [:item
                                   123
                                   ::db.h/mutation-response-swap]}}
         :children     [{:dispatch-key * :key *}]}

    "modify query component when present"
    (-> (db.h/transform-remote {:ref   [:item 123]
                                :state (atom {})}
          (-> {:key 'my-mutation} (fm/returning (atom {}) PureUI)))
        (expand-meta))
    => {:dispatch-key 'my-mutation
        :key          'my-mutation
        :params       {}
        :type         :call
        :query        {::source [:ui/id :ui/value]
                       ::meta   {:queryid                              "fulcro.incubator.workspaces.db-helpers-ws/PureUI"
                                 :fulcro.client.impl.data-fetch/target [:item 123 ::db.h/mutation-response-swap]}}
        :children     [{:type :prop, :dispatch-key :ui/id, :key :ui/id}
                       {:type :prop, :dispatch-key :ui/value, :key :ui/value}]}))

#_(deftest test-gen-pessimistic-mutations
  (assertions
    "just remote"
    (with-redefs [gensym_counter (atom 0)]
      (db.h/gen-pessimistic-mutation 'my-mutation '[args]
        '((remote [_] true))))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env1]
              (clojure.core/let [_ env1]
                (fulcro.incubator.db-helpers/transform-remote
                  env1
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [_] nil))

          nil)

    "generate simple pessimist mutation"
    (with-redefs [gensym_counter (atom 0)]
      (db.h/gen-pessimistic-mutation 'my-mutation '[args]
        '((action [env] nil)
           (remote [_] true))))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env1]
              (clojure.core/let [_ env1]
                (fulcro.incubator.db-helpers/transform-remote
                  env1
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [env] nil))

          nil)

    "handle refresh"
    (with-redefs [gensym_counter (atom 0)]
      (db.h/gen-pessimistic-mutation 'my-mutation '[args]
        '((refresh [_] [:refresh-attr])
           (remote [_] true))))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env1]
              (clojure.core/let [_ env1]
                (fulcro.incubator.db-helpers/transform-remote
                  env1
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [_] nil)
            (refresh [_] [:refresh-attr]))

          nil)

    "generates error handler"
    (with-redefs [gensym_counter (atom 0)]
      (db.h/gen-pessimistic-mutation 'my-mutation '[args]
        '((error-action [env] nil)
           (remote [_] true))))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env1]
              (clojure.core/let [_ env1]
                (fulcro.incubator.db-helpers/transform-remote
                  env1
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [_] nil))

          (fulcro.client.mutations/defmutation my-mutation-error [args]
            (action [env] nil)))

    "generate simple pessimist mutation with pre"
    (with-redefs [gensym_counter (atom 0)]
      (db.h/gen-pessimistic-mutation 'my-mutation '[args]
        '((pre-action [env] nil)
           (action [env] nil)
           (remote [_] true))))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (action [env] nil)
            (remote
              [env1]
              (clojure.core/let [_ env1]
                (fulcro.incubator.db-helpers/transform-remote
                  env1
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [env] nil))

          nil)

    "generate with all handlers"
    (with-redefs [gensym_counter (atom 0)]
      (db.h/gen-pessimistic-mutation 'my-mutation '[args]
        '((pre-action [env] :pre)
           (action [env] :action)
           (error-action [env] :error)
           (remote [_] true))))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (action [env] :pre)
            (remote
              [env1]
              (clojure.core/let [_ env1]
                (fulcro.incubator.db-helpers/transform-remote
                  env1
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [env] :action))

          (fulcro.client.mutations/defmutation my-mutation-error [args]
            (action [env] :error)))))
