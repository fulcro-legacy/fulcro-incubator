(ns fulcro.incubator.spec-helpers
  #?(:cljs (:require-macros [fulcro.incubator.spec-helpers]))
  (:require [ghostwheel.core :as g]
            [clojure.spec.test.alpha :as st]))

#?(:clj (defmacro dev-instrument! [f]
          (when (and (nil? (:ns &env)) (System/getProperty "dev"))
            `(st/instrument ~f))))

(def => :ret)
(def | :st)
(def <- :gen)

#?(:clj (defmacro Defn [& forms]
          (if (:ns &env)                                    ; cljs?
            `(g/>defn ~@forms)
            (let [sym   (first forms)
                  nspc  (some-> (resolve sym) meta :ns str)
                  fqsym (symbol nspc (name sym))]
              `(do
                 (g/>defn ~@forms)
                 (dev-instrument! (quote ~fqsym)))))))

#?(:clj (defmacro Defn- [& forms]
          (if (:ns &env)                                    ; cljs?
            `(g/>defn- ~@forms)
            (let [sym   (first forms)
                  nspc  (some-> (resolve sym) meta :ns str)
                  fqsym (symbol nspc (name sym))]
              `(do
                 (g/>defn- ~@forms)
                 (dev-instrument! (quote ~fqsym)))))))

