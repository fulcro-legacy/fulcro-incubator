(ns fulcro.incubator.client-test-main
  (:require
    [fulcro-spec.selectors :as sel]
    [fulcro-spec.suite :as suite]))

(suite/def-test-suite client-tests {:ns-regex #".*test$"}
  {:default   #{::sel/none :focused}
   :available #{:focused}})

(defn start [] (client-tests))
(defn stop [done] (done))
(defn ^:export init [] (start))
