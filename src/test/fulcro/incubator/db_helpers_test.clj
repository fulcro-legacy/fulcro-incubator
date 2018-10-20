(ns fulcro.incubator.db-helpers-test
  (:require [clojure.test :refer :all]
            [fulcro.incubator.db-helpers :as db.h]
            [fulcro-spec.core :refer [assertions]]))

(deftest test-gen-pessimistic-mutations
  (assertions
    "just remote"
    (#'db.h/gen-pessimistic-mutation 'my-mutation '[args]
      '((remote [_] true)))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env]
              (clojure.core/let [_ env]
                (fulcro.incubator.db-helpers/transform-remote
                  env
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [_] nil))

          nil)

    "generate simple pessimist mutation"
    (#'db.h/gen-pessimistic-mutation 'my-mutation '[args]
      '((action [env] nil)
         (remote [_] true)))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env]
              (clojure.core/let [_ env]
                (fulcro.incubator.db-helpers/transform-remote
                  env
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [env] nil))

          nil)

    "handle refresh"
    (#'db.h/gen-pessimistic-mutation 'my-mutation '[args]
      '((refresh [_] [:refresh-attr])
         (remote [_] true)))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env]
              (clojure.core/let [_ env]
                (fulcro.incubator.db-helpers/transform-remote
                  env
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [_] nil)
            (refresh [_] [:refresh-attr]))

          nil)

    "generates error handler"
    (#'db.h/gen-pessimistic-mutation 'my-mutation '[args]
      '((error-action [env] nil)
         (remote [_] true)))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (remote
              [env]
              (clojure.core/let [_ env]
                (fulcro.incubator.db-helpers/transform-remote
                  env
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [_] nil))

          (fulcro.client.mutations/defmutation my-mutation-error [args]
            (action [env] nil)))

    "generate simple pessimist mutation with pre"
    (#'db.h/gen-pessimistic-mutation 'my-mutation '[args]
      '((pre-action [env] nil)
         (action [env] nil)
         (remote [_] true)))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (action [env] nil)
            (remote
              [env]
              (clojure.core/let [_ env]
                (fulcro.incubator.db-helpers/transform-remote
                  env
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [env] nil))

          nil)

    "generate with all handlers"
    (#'db.h/gen-pessimistic-mutation 'my-mutation '[args]
      '((pre-action [env] :pre)
         (action [env] :action)
         (error-action [env] :error)
         (remote [_] true)))
    => '(do
          (fulcro.client.mutations/defmutation my-mutation [args]
            (action [env] :pre)
            (remote
              [env]
              (clojure.core/let [_ env]
                (fulcro.incubator.db-helpers/transform-remote
                  env
                  (do true)))))

          (fulcro.client.mutations/defmutation my-mutation-ok [args]
            (action [env] :action))

          (fulcro.client.mutations/defmutation my-mutation-error [args]
            (action [env] :error)))))
