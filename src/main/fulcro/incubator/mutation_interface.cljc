(ns fulcro.incubator.mutation-interface
  #?(:cljs (:require-macros fulcro.incubator.mutation-interface))
  (:require [clojure.spec.alpha :as s])
  #?(:clj
     (:import (clojure.lang IFn))))

(def ^:dynamic *checked-mutations* false)

#?(:clj
   (deftype Mutation [sym param-spec]
     IFn
     (invoke [this]
       (this {}))
     (invoke [this args]
       (when (and *checked-mutations* (not (s/valid? param-spec args)))
         (throw (ex-info "Mutation failed spec" {:mutation sym :reason (s/explain param-spec args)})))
       (list sym args)))
   :cljs
   (deftype Mutation [sym param-spec]
     IFn
     (-invoke [this]
       (this {}))
     (-invoke [this args]
       (when (and *checked-mutations* (not (s/valid? param-spec args)))
         (throw (ex-info "Mutation failed spec" {:mutation sym :reason (s/explain param-spec args)})))
       (list sym args))))

#?(:clj
   (defmacro declare-mutation
     "Define a quote-free interface for using the given `target-symbol` in mutations.
     The declared mutation can be used in lieu of the true mutation symbol
     as a way to prevent circular references and add spec checking for the mutation arguments.
     Specs are only checked if used with dynamic var *check-mutations* set to true.

     In IntelliJ, use Resolve-as `def` to get proper IDE integration."
     ([name target-symbol]
      `(def ~name
         (->Mutation ~target-symbol map?)))
     ([name target-symbol spec]
      `(def ~name
         (->Mutation ~target-symbol ~spec)))
     ([name docstring target-symbol spec]
      `(def ~(with-meta name {:doc docstring})
         (->Mutation ~target-symbol ~spec)))))
