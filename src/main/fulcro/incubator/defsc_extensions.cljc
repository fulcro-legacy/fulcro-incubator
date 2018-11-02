(ns fulcro.incubator.defsc-extensions
  #?(:cljs (:require-macros fulcro.incubator.defsc-extensions))
  (:require
    [fulcro.client.primitives :as prim]
    [clojure.pprint :refer [pprint]]
    [cljs.analyzer :as ana]
    [clojure.spec.alpha :as s]))

#?(:clj
   (defn- replace-and-validate-fn
     "Replace the first sym in a list (the function name) with the given symbol.

env - the macro &env
sym - The symbol that the lambda should have
external-args - A sequence of argmuments that the user should not include, but that you want to be inserted in the external-args by this function.
user-arity - The number of external-args the user should supply (resulting user-arity is (count external-args) + user-arity).
fn-form - The form to rewrite
sym - The symbol to report in the error message (in case the rewrite uses a different target that the user knows)."
     [env sym external-args user-arity fn-form]
     (#'prim/replace-and-validate-fn env sym external-args user-arity fn-form)))

#?(:clj
   (defn report [env input-form error-message]
     (throw (ana/error (merge env (meta input-form)) error-message))))

#?(:clj
   (defn resolve* [env input-form psym]
     (try
       (if-let [v (-> psym (resolve) var-get)]
         v
         (report env input-form (str "Cannot resolve " psym ". Make sure your protocol is in a *cljc* file, and is required ")))
       (catch Throwable _
         (report env input-form (str "Cannot resolve " psym ". Make sure your protocol is in a *cljc* file, and is required "))))))

#?(:clj
   (defn morph-option-to-method [env this-sym target-name required-args user-fn]
     (let [user-arity (dec (count required-args))]
       (replace-and-validate-fn env target-name [this-sym] user-arity user-fn))))

#?(:clj
   (defn morph-options-to-methods [env this-sym methods incoming-defsc-options]
     (mapv
       (fn [[k {:keys [name arglists]}]]
         (when (not= 1 (count arglists))
           (report env incoming-defsc-options (str "Extended defsc does not yet support multi-arity functions on protocols. Problem method was: " name)))
         (if-let [user-fn (get incoming-defsc-options k)]
           (morph-option-to-method env this-sym name (first arglists) user-fn)
           (report env incoming-defsc-options (str "The options do not include the required option: " k))))
       methods)))

#?(:clj
   (defn emit-protocol
     [env this-sym protocol-sym incoming-defsc-options static?]
     (when-not (map? incoming-defsc-options)
       (report env incoming-defsc-options "Syntax Error: Extended defsc requires an options map immediately after the parameter list."))
     (let [protocol        (resolve* env incoming-defsc-options protocol-sym)
           methods         (:sigs protocol)
           preamble        (if static? ['static protocol-sym] [protocol-sym])
           refined-methods (morph-options-to-methods env this-sym methods incoming-defsc-options)]
       (when (empty? methods)
         (report env incoming-defsc-options (str "Cannot extend support for empty (or missing) protocol " protocol)))
       `[~@preamble
         ~@refined-methods])))

(s/def ::protocol-specifier (s/tuple symbol? boolean?))
(s/def ::supported-protocols (s/coll-of ::protocol-specifier :kind vector?))

#?(:clj
   (defn defsc-options->protocols
     "supported-protocols is a vector of vectors:

     ```
     [[Protocol static?] ...]
     ```

     where static? is a boolean indicator for how to treat the protocol with respect to defsc.
     "
     [env thissym options supported-protocols]
     (when-not (s/valid? ::supported-protocols supported-protocols)
       (report env supported-protocols "Supported protocols is not a vector of vectors of sym/boolean."))
     (let [
           existing-protocols (get options :protocols [])]
       (reduce
         (fn [protocols [protocol-sym static?]]
           (into protocols (emit-protocol env thissym protocol-sym options static?)))
         existing-protocols supported-protocols))))

#?(:clj
   (defn strip-protocol-options
     "options is the defsc options map passed to the defsc macro.
     supported-protocols is a vector of vectors:

     ```
     [[Protocol static?] ...]
     ```

     where static? is a boolean indicator for how to treat the protocol with respect to defsc.
     "
     [env options supported-protocols]
     (when-not (s/valid? ::supported-protocols supported-protocols)
       (report env supported-protocols "Supported protocols is not a vector of vectors of sym/boolean."))
     (reduce (fn [options [protocol _]]
               (let [p  (resolve* env options protocol)
                     ks (-> p :sigs keys)]
                 (apply dissoc options ks)))
       options
       supported-protocols)))

#?(:clj
   (defn rewrite-defsc-protocols [env defsc-forms supported-protocols]
     (when-not (s/valid? ::supported-protocols supported-protocols)
       (report env supported-protocols "Supported protocols is not a vector of vectors of sym/boolean."))
     (let [{:keys [sym doc arglist parsed-options body]} (s/conform :fulcro.client.primitives.defsc/args defsc-forms)
           options     (first (filter map? defsc-forms))
           [thissym _ _ _] arglist
           protocols   (defsc-options->protocols env thissym options supported-protocols)
           new-options (cond-> (strip-protocol-options env options supported-protocols)
                         (seq protocols) (assoc :protocols protocols))
           new-forms   (keep identity [`prim/defsc sym doc arglist new-options])]
       (concat new-forms body))))

#?(:clj
   (defmacro defextended-defsc
     "Create a macro that works like defsc, but accepts protocol methods as keyword options and transforms them into
     the proper protocol support.

     Usage:

     ```
     (defextended-defsc defsc-router supported-protocols)
     ```

     where supported-protocols is a vector of tuples:

     ```
     [[protocol-symbol static?] ...]
     ```

     NOTES:
     - You MUST run this macro in a CLJ or the CLJ side of a CLJC file (ideally the latter).
     - You should also use a self-referencing CLJC file with `require-macros` so your users will get better usage behavior.
     - You MUST define the protocol(s) in a CLJC file (both CLJ and CLJS must be able to see your protocol, even if you
     only plan to use it in CLJS).
     "
     [macro-name supported-protocols]
     (let [f `(println "Hi")]
       `(defmacro ~macro-name [& ~'forms]
          (let [~'result (rewrite-defsc-protocols ~'&env ~'forms ~supported-protocols)]
            ~'result)))))



