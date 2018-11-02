(ns fulcro.incubator.defsc-foo
  (:require
    [fulcro.incubator.other-protocol :refer [Other]]
    [fulcro.incubator.defsc-extensions :refer [defextended-defsc]]))

(defextended-defsc defsc-foo [[`Other true]])

(macroexpand-1 '(defsc-foo TestComponent [this props]
    {:special (fn [] (js/console.log "Did special thing!"))}
    (dom/div
      (dom/button {:onClick (fn [] (foo/special TestComponent))} "Try it!!!"))))
