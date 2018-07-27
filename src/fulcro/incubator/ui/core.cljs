(ns fulcro.incubator.ui.core
  (:require [fulcro.client.localized-dom :as dom]))

(defn js-spread
  "Use this to merge JS props with your Clojure props, this is usually needed when you
  use React components written in pure react that use child functions to render.

  Like JSX usage: <Button as={Overlay.Show} {...overlay}>
  Becomes: (button (js-spread {:as Overlay.Show} overlay))"
  [props js-props]
  (js/Object.assign (js-obj) (clj->js props) js-props))

(defn component-factory-localized
  "Wrap a react component with localized css support (like on dom/*)"
  [component]
  (fn [& args]
    (if (keyword? (first args))
      (dom/macro-create-element component (next args) (first args))
      (dom/macro-create-element component args))))

(defn component-factory-simple
  "Make a factory to build a React instance from a React class."
  [component]
  (fn
    ([] (dom/create-element component))
    ([props] (dom/create-element component (clj->js props)))
    ([props & children] (apply dom/create-element component (clj->js props) children))))
