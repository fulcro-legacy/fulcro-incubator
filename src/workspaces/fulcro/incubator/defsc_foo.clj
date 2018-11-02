(ns fulcro.incubator.defsc-foo
  (:require
    [fulcro.incubator.other-protocol :refer [Other]]
    [fulcro.incubator.defsc-extensions :refer [defextended-defsc]]))

(defextended-defsc defsc-foo [[`Other true]])

