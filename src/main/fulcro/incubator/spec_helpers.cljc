(ns fulcro.incubator.spec-helpers
  #?(:cljs (:require-macros fulcro.incubator.spec-helpers))
  (:require
    [cljs.env]
    [clojure.test]
    [clojure.spec.test.alpha :as st]
    [clojure.set :refer [union difference map-invert]]
    [clojure.string :as str]
    [clojure.walk :as walk]
    [clojure.pprint :refer [pprint]]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    #?@(:clj  [[clojure.core.specs.alpha]]
        :cljs [[cljs.core.specs.alpha :include-macros true]])))

(def ^:private ^:dynamic *unsafe-bound-ops* #{})

(defn cljs-env? [env] (boolean (:ns env)))

(defn clj->cljs
  ([form]
   (clj->cljs form true))
  ([form strip-core-ns]
   (let [ns-replacements   (cond-> {"clojure.core"            "cljs.core"
                                    "clojure.test"            "cljs.test"
                                    "clojure.spec.alpha"      "cljs.spec.alpha"
                                    "clojure.spec.test.alpha" "cljs.spec.test.alpha"
                                    "orchestra.spec.test"     "orchestra-cljs.spec.test"
                                    "clojure.spec.gen.alpha"  "cljs.spec.gen.alpha"}
                             strip-core-ns (merge {"clojure.core" nil
                                                   "cljs.core"    nil}))
         replace-namespace #(if-not (qualified-symbol? %)
                              %
                              (let [nspace (namespace %)]
                                (if (contains? ns-replacements nspace)
                                  (symbol (get ns-replacements nspace) (name %))
                                  %)))]
     (walk/postwalk replace-namespace form))))

(defn- count-args
  "Returns a tuple with the number of regular and non-variadic arguments."
  [conformed-args]
  [(count (:args conformed-args))
   (if (:varargs conformed-args) 1 0)])

;;;; Operators

;; It doesn't actually matter what these are bound to, they are stripped by the macros
;; they're used in and never end up in the final code. This is just so they can be used
;; without '=> cannot be resolved' errors in the IDE.
(def => :ret)
(def | :st)
(def <- :gen)

(defmacro ? [& forms]
  (cond-> `(s/nilable ~@forms)
    (cljs-env? &env) clj->cljs))


;;;; Specs


(s/def ::trace #{0 1 2 3 4 5 true})
(s/def ::trace-color (s/or :keyword keyword?
                       :literal (s/and string?
                                  #(re-matches #"#[a-fA-F0-9]+" %)
                                  #(or (= (count %) 7)
                                     (= (count %) 4)))))
(s/def ::check boolean?)
(s/def ::check-coverage boolean?)
(s/def ::ignore-fx boolean?)
(s/def ::num-tests nat-int?)
(s/def ::num-tests-ext nat-int?)
(s/def ::extensive-tests boolean?)
(s/def ::instrument boolean?)
(s/def ::outstrument boolean?)
(s/def ::extrument (s/nilable (s/coll-of qualified-symbol? :kind vector?)))


;; These are lifted straight from clojure.core.specs.alpha, because it
;; didn't seem possible to access them directly in the original namespace.

(s/def ::local-name (s/and simple-symbol? #(not= '& %)))

;; sequential destructuring

(s/def ::seq-binding-form
  (s/and vector?
    (s/cat :elems (s/* ::binding-form)
      :rest (s/? (s/cat :amp #{'&} :form ::binding-form))
      :as (s/? (s/cat :as #{:as} :sym ::local-name)))))

;; map destructuring

(s/def ::keys (s/coll-of ident? :kind vector?))
(s/def ::syms (s/coll-of symbol? :kind vector?))
(s/def ::strs (s/coll-of simple-symbol? :kind vector?))
(s/def ::or (s/map-of simple-symbol? any?))
(s/def ::as ::local-name)

(s/def ::map-special-binding
  (s/keys :opt-un [::as ::or ::keys ::syms ::strs]))

(s/def ::map-binding (s/tuple ::binding-form any?))

(s/def ::ns-keys
  (s/tuple
    (s/and qualified-keyword? #(-> % name #{"keys" "syms"}))
    (s/coll-of simple-symbol? :kind vector?)))

(s/def ::map-bindings
  (s/every (s/or :mb ::map-binding
             :nsk ::ns-keys
             :msb (s/tuple #{:as :or :keys :syms :strs} any?))
    :into {}))

(s/def ::map-binding-form (s/merge ::map-bindings ::map-special-binding))

(s/def ::binding-form
  (s/or :sym ::local-name
    :seq ::seq-binding-form
    :map ::map-binding-form))

;;; Function and >defn specs

(s/def ::arg-list
  (s/and vector?
    (s/cat :args (s/* ::binding-form)
      :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))

(s/def ::pred-arg-list
  (s/and vector?
    (s/cat :args (s/* (s/or :sym ::local-name)))))

(s/def ::anon-args+body
  (s/cat :args ::arg-list
    :body (s/* any?)))

(s/def ::anon-fn
  (s/and seq?
    (s/cat :op #{'fn* 'fn}
      :name (s/? simple-symbol?)
      :bs (s/alt :arity-1 ::anon-args+body
            :arity-n (s/+ (s/spec ::anon-args+body))))))

(s/def ::pred-fn
  (s/and seq?
    (s/cat :op #{'fn* 'fn}
      :name (s/? simple-symbol?)
      :args ::pred-arg-list
      :body any?)))

(s/def ::spec-elem
  (s/or :set set?
    :pred-sym (s/and symbol?
                (complement #{'| '=>})
                ;; REVIEW: should the `?` be a requirement?
                #(str/ends-with? (str %) "?"))
    :gspec (s/or :nilable-gspec ::nilable-gspec :gspec ::gspec)
    :spec-key qualified-keyword?
    :fun ::pred-fn
    :list seq?))

(s/def ::such-that-op #{:st '|})
(s/def ::ret-op #{:ret '=>})
(s/def ::gen-op #{:gen '<-})

(s/def ::gspec
  (s/and vector?
    (s/cat :args (s/? (s/cat :args (s/+ ::spec-elem)
                        :args-such-that (s/? (s/cat :op ::such-that-op
                                               :preds (s/+ ::pred-fn)))))
      :ret-op ::ret-op
      :ret ::spec-elem
      :fn-such-that (s/? (s/cat :op ::such-that-op
                           :preds (s/+ ::pred-fn)))
      :gen (s/? (s/cat :op ::gen-op
                  :gen-fn (s/? (some-fn seq? symbol?)))))))

(s/def ::nilable-gspec
  (s/and vector?
    (s/cat :maybe #{'? 's/nilable}
      :gspec ::gspec)))

(s/def ::prepost (s/map-of #{:pre :post}
                   (s/coll-of seq?
                     :kind vector?
                     :distinct true)))

(s/def ::args+body
  (s/cat :args ::arg-list
    :body (s/alt :prepost+body (s/cat :prepost ::prepost
                                 :body (s/+ any?))
            :body (s/* any?))))

(s/def ::args+gspec+body
  (s/&
    (s/cat :args ::arg-list
      :gspec (s/nilable ::gspec)
      :body (s/alt :prepost+body (s/cat :prepost ::prepost
                                   :body (s/+ any?))
              :body (s/* any?)))
    (fn arg-specs-match-param-count? [{:keys [args gspec]}]
      (if-not gspec
        true
        (let [argcount  (->> args count-args (apply +))
              spec-args (:args gspec)]
          (if spec-args
            (-> spec-args :args count (= argcount))
            (= argcount 0)))))))

(s/def ::fdef
  (s/and seq?
    (s/cat :op #{'cljs.spec.alpha/fdef 'clojure.spec.alpha/fdef}
      :name symbol?
      :args-op #{:args}
      :args seq?                                            ; REVIEW - refine?
      :ret (s/? (s/cat :ret-op #{:ret}
                  :ret ::spec-elem))
      :fun (s/? (s/cat :fn-op #{:fn}
                  :pred seq?)))))                           ; REVIEW - refine?

(s/def ::defn
  (s/and seq?
    (s/cat :op #{'defn 'defn-}
      :name simple-symbol?
      :docstring (s/? string?)
      :meta (s/? map?)
      :bs (s/alt :arity-1 ::args+body
            :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                       :attr (s/? map?))))))

(s/def ::deftest
  (s/and seq?
    (s/cat :op #{'clojure.test/deftest 'cljs.test/deftest}
      :name symbol?
      :body any?)))

;; FIXME: Update the spec - doesn't match current `desugar-defn` output
(s/def ::desugared->defn
  (s/and seq?
    (s/cat :do #{'do}
      :defn ::defn
      :fdef (s/nilable ::fdef)
      :deftest (s/nilable ::deftest))))

;;; Side effect detection specs

(s/def ::threading-macro-op
  #{'-> '->> 'as-> 'cond-> 'cond->> 'some-> 'some->>
    '*-> '*->> '*as-> '*cond-> '*cond->> '*some-> '*some->>})

(s/def ::binding-op
  #{'let 'for 'doseq 'binding})

(s/def ::single-function-composition
  #{'partial 'fnil})

(s/def ::safe-single-function-composition
  #{'memoize 'complement})

(s/def ::multi-function-composition
  #{'comp})

(s/def ::safe-multi-function-composition
  #{'juxt 'every-pred 'some-fn})

(s/def ::function-application
  #{'apply 'map 'fmap 'map-indexed 'reduce})

(s/def ::safe-function-application
  #{'mapcat 'reduce-kv 'mapv 'reductions 'iterate 'keep 'keep-indexed
    'remove 'filter 'filterv 'take-while 'drop-while
    'sort 'sort-by 'sorted-map-by 'group-by 'merge-with})

(s/def ::unsafe-clj-block #{'do
                            'doseq
                            'dotimes})

;; REVIEW: maybe move the re-frame stuff out of here
(s/def ::unsafe-clj-call #{'dorun
                           'repeatedly
                           'dispatch
                           'js-delete
                           'aset})

(s/def ::unsafe-clj-comp
  (s/alt :single-fn (s/cat :composition (s/alt :generic ::single-function-composition
                                          :safe ::safe-single-function-composition)
                      :unsafe-op ::unsafe-op
                      :rest ::rest)
    :multi-fn (s/cat :composition (s/alt :generic ::multi-function-composition
                                    :safe ::safe-multi-function-composition)
                :some-unsafe-ops ::some-unsafe-ops
                :rest ::rest)))

(let [bang-suffix? #(str/ends-with? (str %) "!")]
  (s/def ::bang-suffix (every-pred symbol? bang-suffix?))
  (s/def ::unsafe-op
    (s/alt :bang-suffix ::bang-suffix
      :unsafe-anon-fn (s/and seq?
                        (s/alt :unsafe-body (s/cat :fun #{'fn 'fn*}
                                              :name (s/? simple-symbol?)
                                              :args ::arg-list
                                              :body ::unsafe-form)
                          :unsafe-name (s/cat :fun #{'fn 'fn*}
                                         :name (every-pred simple-symbol? bang-suffix?)
                                         :args ::arg-list
                                         :body any?)))
      :unsafe-clj-call ::unsafe-clj-call
      :unsafe-clj-comp (s/spec ::unsafe-clj-comp)
      :unsafe-bound-call #(contains? *unsafe-bound-ops* %)
      :multi-form-op (s/cat :op #{'when 'when-not 'when-let 'when-first
                                  'when-some 'let 'binding}
                       :pred-or-bindings any?
                       :fx (s/+ any?)
                       :return any?))))

(s/def ::safe-op #(not (s/valid? ::unsafe-op (list %))))

(s/def ::some-unsafe-ops (s/+ (s/cat :skipped-ops (s/* ::safe-op)
                                :unsafe-op ::unsafe-op)))

(s/def ::rest (s/* any?))

(s/def ::some-unsafe-bindings
  (s/and vector?
    (s/+ (s/cat :skipped-bindings (s/* (s/cat :binding ::binding-form
                                         :value ::safe-op))
           :unsafe-binding (s/cat :binding ::binding-form
                             :value ::unsafe-op)))))

(s/def ::unsafe-form
  ;; REVIEW: maybe make sure we are only matching on the simple symbol part
  (s/or :unsafe-block (s/and seq?
                        (s/cat :unsafe-clj-block ::unsafe-clj-block
                          :rest ::rest))

    :unsafe-call
    (s/and seq?
      (s/alt :direct (s/cat :application
                       (s/? (s/alt :generic ::function-application
                              :safe ::safe-function-application))
                       :unsafe-op ::unsafe-op
                       :rest ::rest)
        :threading (s/cat :threading-macro-op ::threading-macro-op
                     :threaded-form any?
                     :some-unsafe-ops ::some-unsafe-ops
                     :rest ::rest)
        :update (s/cat :update #{'update 'update-in}
                  :map any?
                  :path any?
                  :unsafe-op ::unsafe-op
                  :rest ::rest)))

    :unsafe-composition (s/and seq? ::unsafe-clj-comp)
    :unsafe-binding (s/and seq?
                      (s/cat :binding-op ::binding-op
                        :bindings ::some-unsafe-bindings
                        :rest ::rest))
    :unsafe-argument (s/and seq?
                       (s/cat :fun ::safe-op
                         :some-unsafe-ops ::some-unsafe-ops
                         :rest ::rest))
    #_::unsafe-something #_(s/spec (s/cat ::some-unsafe-ops ::some-unsafe-ops
                                     ::rest ::rest))))


;;;; Main code generating functions

(defn- unscrew-vec-unform
  "Half-arsed workaround for spec bugs CLJ-2003 and CLJ-2021."
  [unformed-arg]
  (if-not (sequential? unformed-arg)
    unformed-arg
    (let [malformed-seq-destructuring? (every-pred seq? (comp #{:as '&} first))
          [unformed malformed] (split-with (complement malformed-seq-destructuring?) unformed-arg)]
      (vec (concat unformed (apply concat malformed))))))

(defn- gspec->fspec*
  [conformed-arg-list conformed-gspec anon-fspec? multi-arity-args? nilable?]
  (let [{argspec-def              :args
         retspec                  :ret
         fn-such-that             :fn-such-that
         {:keys [gen-fn] :as gen} :gen}
        conformed-gspec]
    (if (and anon-fspec?
          argspec-def
          (not gen)
          (some #{'any?} (-> argspec-def :args vals)))
      (if nilable? `(s/nilable ifn?) `ifn?)
      (let [extract-spec
            (fn extract-spec [[spec-type spec]]
              (if (= spec-type :gspec)
                (if (= (key spec) :nilable-gspec)
                  (gspec->fspec* nil (-> spec val :gspec) true false true)
                  (gspec->fspec* nil (val spec) true false false))
                spec))

            named-conformed-args
            (when argspec-def
              (let [all-args     (remove nil? (concat (:args conformed-arg-list)
                                                [(-> conformed-arg-list :varargs :form)]))
                    gen-arg-name (fn [index] (str "arg" (inc index)))
                    gen-name     (fn [index [arg-type arg :as full-arg]]
                                   (let [arg-name (if-not arg-type
                                                    (gen-arg-name index)
                                                    (case arg-type
                                                      :sym arg
                                                      :seq (or (-> arg :as :sym)
                                                             (gen-arg-name index))
                                                      :map (or (-> arg :as)
                                                             (gen-arg-name index))))]
                                     [(keyword arg-name) full-arg]))]
                (map-indexed gen-name (or (seq all-args)
                                        (-> argspec-def :args count (repeat nil))))))

            arg-binding-map
            (if-not conformed-arg-list
              {}
              (if (every? #(= (-> % second key) :sym) named-conformed-args)
                `{:keys ~(vec (map #(-> % first name symbol) named-conformed-args))}
                (->> named-conformed-args
                  (map (fn [[arg-key conformed-arg]]
                         [(->> conformed-arg (s/unform ::binding-form) unscrew-vec-unform)
                          arg-key]))
                  (into {}))))

            process-arg-pred
            (fn process-arg-pred [{:keys [name args body]}]
              (let [bindings (if-let [anon-arg (some-> args :args first second)]
                               (assoc arg-binding-map :as anon-arg)
                               arg-binding-map)]
                (remove nil? `(fn ~name [~bindings] ~body))))

            processed-args
            (if-not argspec-def
              `(s/cat)
              (let [wrapped-params (->> argspec-def
                                     :args
                                     (map extract-spec)
                                     (interleave (map first named-conformed-args))
                                     (cons `s/cat))]
                (if-let [args-such-that (:args-such-that argspec-def)]
                  (->> args-such-that
                    :preds
                    (map process-arg-pred)
                    (list* `s/and wrapped-params))
                  wrapped-params)))

            process-ret-pred
            (fn process-ret-pred [{:keys [name args body]}]
              (let [anon-arg       (some-> args :args first second)
                    ret-sym        (gensym "ret__")
                    bindings       [{(if multi-arity-args?
                                       ['_ arg-binding-map]
                                       arg-binding-map) :args
                                     ret-sym            :ret}]
                    processed-body (if anon-arg
                                     (walk/postwalk-replace {anon-arg ret-sym} body)
                                     body)]
                (remove nil? `(fn ~name ~bindings ~processed-body))))

            fn-spec
            (when fn-such-that
              (let [processed-ret-preds (map process-ret-pred (:preds fn-such-that))]
                (if (next processed-ret-preds)
                  (cons `s/and processed-ret-preds)
                  (first processed-ret-preds))))

            final-fspec
            (concat (when anon-fspec? [`s/fspec])
              [:args processed-args]
              [:ret (extract-spec retspec)]
              (when fn-spec [:fn fn-spec])
              (when gen-fn [:gen gen-fn]))]
        (if nilable? `(s/nilable ~final-fspec) final-fspec)))))


;; TODO make sure we check whether the variadic bodies are legit
;; Can not have more than one
;; Can not have one with more regular args than the variadic one
;; To what extent does the compiler already check this?
(let [get-fspecs    (fn [fn-body]
                      (let [[param-count variadic] (-> fn-body :args count-args)
                            gspec (or (:gspec fn-body)
                                    (s/conform ::gspec
                                      (vec (concat (repeat param-count 'any?)
                                             (when (> variadic 0)
                                               `[(s/* any?)])
                                             '[=> any?]))))]
                        [(->> (if (> variadic 0) "n" param-count)
                           (str "arity-")
                           keyword)
                         (gspec->fspec* (:args fn-body) gspec false true false)]))
      get-spec-part (fn [part spec]
                      (->> spec
                        (drop-while (complement #{part}))
                        second))]
  (defn- generate-fspec-body [fn-bodies]
    (case (key fn-bodies)
      :arity-1
      (when-let [gspec (-> fn-bodies val :gspec)]
        (gspec->fspec* (-> fn-bodies val :args) gspec false false false))

      :arity-n
      (when (some :gspec (val fn-bodies))
        (let [fspecs           (map get-fspecs (val fn-bodies))
              arg-specs        (mapcat (fn [[arity spec]]
                                         [arity (or (get-spec-part :args spec) `empty?)])
                                 fspecs)
              fn-param         (gensym "p1__")
              multi-ret-specs  (when (->> fspecs
                                       (map #(get-spec-part :ret (second %)))
                                       distinct
                                       count
                                       (not= 1))
                                 (mapcat (fn [[arity spec]]
                                           [arity `(s/valid? ~(get-spec-part :ret spec)
                                                     (:ret ~fn-param))])
                                   fspecs))
              get-fn-clause    (partial get-spec-part :fn)
              fn-specs         (when (->> fspecs (map second) (some get-fn-clause))
                                 (mapcat (fn [[arity spec]]
                                           [arity (if-let [fn-spec (get-fn-clause spec)]
                                                    `(s/valid? ~fn-spec ~fn-param)
                                                    true)])
                                   fspecs))
              ;; NOTE: destructure args and ret in the arg vec
              multi-ret-clause (when multi-ret-specs
                                 `(fn ~'valid-multi-arity-ret? [~fn-param]
                                    (case (-> ~fn-param :args key)
                                      ~@multi-ret-specs)))
              multi-fn-clause  (when fn-specs
                                 `(fn ~'valid-multi-arity-fn? [~fn-param]
                                    (case (-> ~fn-param :args key)
                                      ~@fn-specs)))]
          ;; Using s/or here even though s/alt seems to be more common
          ;; for multi-arity specs in the wild. The spec error reporting
          ;; is much better and it's immediately clear what didn't match.
          (concat [:args `(s/or ~@arg-specs)]
            (when-not multi-ret-clause
              [:ret (get-spec-part :ret (-> fspecs first second))])
            (when (or multi-ret-clause multi-fn-clause)
              [:fn (if multi-fn-clause
                     (if multi-ret-clause
                       `(s/and ~multi-ret-clause ~multi-fn-clause)
                       multi-fn-clause)
                     multi-ret-clause)])))))))

(def ^:private spec-op->type
  (let [map-prot     "cljs.core.IMap"
        coll-prot    "cljs.core.ICollection"
        ;; Needed because Closure compiler/JS doesn't consider strings seqable
        seqable-prot "(cljs.core.ISeqable|string)"]
    {'number?      "number"
     'integer?     "number"
     'int?         "number"
     'nat-int?     "number"
     'pos-int?     "number"
     'neg-int?     "number"
     'float?       "number"
     'double?      "number"
     'int-in       "number"
     'double-in    "number"

     'string?      "string"

     'boolean?     "boolean"

     'keys         map-prot
     'map-of       map-prot
     'map?         map-prot
     'merge        map-prot

     'set?         "cljs.core.ISet"
     'vector?      "cljs.core.IVector"
     'tuple        "cljs.core.IVector"
     'seq?         "cljs.core.ISeq"
     'seqable?     seqable-prot
     'associative? "cljs.core.IAssociative"
     'atom?        "cljs.core.IAtom"

     'coll-of      coll-prot
     'every        coll-prot

     'keyword?     "cljs.core.Keyword"
     'ifn?         "cljs.core.IFn"
     'fn?          "Function"}))

(declare get-gspec-type)

(defn- get-type [recursive-call conformed-spec-elem]
  (let [[spec-type spec-def] conformed-spec-elem

        spec-op
        ;; REVIEW: This kinda wants to be a multi-method when it grows up.
        (case spec-type
          :list (let [op (-> spec-def first name symbol)]
                  (cond
                    (#{'nilable '?} op) (concat (->> spec-def
                                                  second
                                                  (s/conform ::spec-elem)
                                                  (get-type true))
                                          [::nilable])
                    (#{'* '+} op) (concat (->> spec-def
                                            second
                                            (s/conform ::spec-elem)
                                            (get-type true))
                                    [::variadic])
                    (#{'and} op) [(-> spec-def second)]     ; TODO
                    (#{'coll-of 'every} op) [(or (->> spec-def
                                                   (drop-while (complement #{:kind}))
                                                   second)
                                               op)]
                    :else [op]))
          ;;TODO support (some-fn and (s/or
          :gspec (let [gspec-def (val spec-def)]
                   (if (= (key spec-def) :nilable-gspec)
                     [(get-gspec-type (:gspec gspec-def)) ::nilable]
                     [(get-gspec-type gspec-def)]))
          :pred-sym [spec-def]
          [nil])]
    (if recursive-call
      spec-op
      (if-let [js-type (spec-op->type (first spec-op))]
        (let [modifiers (set (rest spec-op))]
          (as-> js-type t
            (str (if (::nilable modifiers) "?" "!") t)
            (str (when (::variadic modifiers) "...") t)))
        "*"))))

(defn- get-gspec-type [conformed-gspec]
  (let [argspec-def (:args conformed-gspec)
        args-jstype (if-not argspec-def
                      ""
                      (->> (-> conformed-gspec :args :args)
                        (map (partial get-type false))
                        (str/join ", ")))
        ret-jstype  (get-type false (:ret conformed-gspec))]
    (str "function(" args-jstype "): " ret-jstype)))

(defn- generate-type-annotations [conformed-bs]
  (case (key conformed-bs)
    :arity-1 (when-let [gspec (-> conformed-bs val :gspec)]
               {:jsdoc [(str "@type {" (get-gspec-type gspec) "}")]})
    ;; REVIEW: There doesn't seem to be a way to get valid annotations for args of
    ;; multi-arity functions and attempts to just annotate the return value(s) failed
    ;; as well. It wasn't possible to put together an annotation which was both
    ;; considered valid and resulted in a successful type check.
    :arity-n nil #_(when-let [ret-types (as-> (val conformed-bs) x
                                          (map #(get-type false (-> % :gspec :ret)) x)
                                          (distinct x)
                                          (when (not-any? #{"*" "?"} x) x))]
                     {:jsdoc [(str "@return {" (cs/join "|" ret-types) "}")]})))

(defn- generate-fdef
  [forms]
  (let [{:keys [name bs]} (s/conform ::>fdef-args forms)]
    (case (key name)
      :sym `(s/fdef ~(val name) ~@(generate-fspec-body bs))
      :key `(s/def ~(val name) (s/fspec ~@(generate-fspec-body bs))))))

(let [process-defn-body
      (fn [cfg fspec args+gspec+body]
        (let [{:keys [env fn-name]} cfg
              {:keys [args body]} args+gspec+body
              [prepost orig-body-forms] (case (key body)
                                          :prepost+body [(-> body val :prepost)
                                                         (-> body val :body)]
                                          :body [nil (val body)])
              process-arg (fn [[arg-type arg]]
                            (as-> arg arg
                              (case arg-type
                                :sym [arg-type arg]
                                :seq [arg-type (update arg :as #(or % {:as :as :sym (gensym "arg_")}))]
                                :map [arg-type (update arg :as #(or % (gensym "arg_")))])))
              ;; NOTE: usage of extract-arg isn't elegant, there's duplication, refactor
              extract-arg (fn [[arg-type arg]]
                            (case arg-type
                              :sym arg
                              :seq (get-in arg [:as :sym])
                              :map (:as arg)
                              nil))
              unform-arg  #(->> % (s/unform ::binding-form) unscrew-vec-unform)
              reg-args    (->> args :args (map process-arg))
              var-arg     (some-> args :varargs :form process-arg)
              arg-list    (vec (concat (map unform-arg reg-args)
                                 (when var-arg ['& (unform-arg var-arg)])))
              body-forms  (if (and fspec (every? nil? orig-body-forms))
                            ;; TODO error handling when specs too fancy for stub auto-generation
                            [`(apply (-> ~fspec s/gen gen/generate)
                                ~@(map extract-arg reg-args) ~(extract-arg var-arg))]
                            orig-body-forms)]
          (remove nil? `(~arg-list ~prepost ~@body-forms))))]
  (defn- generate-defn
    [forms private env]
    (let [conformed-gdefn   (s/conform ::>defn-args forms)
          fn-deftype        (if private 'defn- 'defn)
          fn-bodies         (:bs conformed-gdefn)
          arity             (key fn-bodies)
          fn-name           (:name conformed-gdefn)
          docstring         (:docstring conformed-gdefn)
          meta-map          (merge (:meta conformed-gdefn)
                              (generate-type-annotations fn-bodies))
          ;;; Code generation
          fdef-body         (generate-fspec-body fn-bodies)
          fdef              (when fdef-body `(s/fdef ~fn-name ~@fdef-body))

          individual-arity-fspecs
                            (map (fn [{:keys [args gspec]}]
                                   (when gspec
                                     (gspec->fspec* args gspec true false false)))
                              (val fn-bodies))

          process-fn-bodies (fn []
                              (let [process-cfg {:env     env
                                                 :fn-name fn-name}]
                                (case arity
                                  :arity-1 (->> fn-bodies val (process-defn-body process-cfg `(s/fspec ~@fdef-body)))
                                  :arity-n (map (partial process-defn-body process-cfg)
                                             individual-arity-fspecs
                                             (val fn-bodies)))))
          main-defn         (remove nil? `(~fn-deftype
                                            ~fn-name
                                            ~docstring
                                            ~meta-map
                                            ~@(process-fn-bodies)))]
      `(do ~fdef ~main-defn))))

;;;; Main macros and public API

(s/def ::>defn-args
  (s/and seq?                                               ; REVIEW
    (s/cat :name simple-symbol?
      :docstring (s/? string?)
      :meta (s/? map?)
      :bs (s/alt :arity-1 ::args+gspec+body
            ;; TODO: add tail-attr-map support after this
            :arity-n (s/+ (s/and seq? ::args+gspec+body))))))

(s/fdef >defn
  :args ::>defn-args
  :ret ::desugared->defn)

(defmacro >defn
  "Like defn, but requires a (nilable) gspec definition and generates
  additional s/fdef and instrumentation code if JVM -Dinstrument=true is set or
  CLJS compiler option [:options :external-config :fulcro.incubator.spec-helpers/instrument?] is true."
  {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
  [& forms]
  (cond-> (remove nil? (generate-defn forms false &env))
    (cljs-env? &env) clj->cljs))

(s/fdef >defn-
  :args ::>defn-args
  :ret ::desugared->defn)

;; NOTE: lots of duplication - refactor this to set/pass ^:private differently and call >defn
(defmacro >defn-
  "Like defn-, but requires a (nilable) gspec definition and generates
  additional s/fdef, generative tests, instrumentation code, an
  fspec-based stub, and/or tracing code, depending on the configuration
  metadata and the existence of a valid gspec and non-nil body."
  {:arglists '([name doc-string? attr-map? [params*] gspec prepost-map? body?]
                [name doc-string? attr-map? ([params*] gspec prepost-map? body?) + attr-map?])}
  [& forms]
  (cond-> (remove nil? (generate-defn forms true &env))
    (cljs-env? &env) clj->cljs))

(s/def ::>fdef-args
  (s/and seq?                                               ;REVIEW
    (s/cat :name (s/or :sym symbol? :key qualified-keyword?)
      :bs (s/alt :arity-1 ::args+gspec+body
            :arity-n (s/+ (s/and seq? ::args+gspec+body))))))

(s/fdef >fdef
  :args ::>fdef-args
  :ret ::fdef)

(defmacro >fdef
  "Defines an fspec, supports gspec syntax. `name` can be a symbol
  or a qualified keyword, depending on whether the fspec is meant
  to be registered as a top-level fspec (=> s/fdef fn-sym ...) or
  used in other specs (=> s/def ::spec-keyword (s/fspec ...))."
  {:arglists '([name [params*] gspec]
                [name ([params*] gspec) +])}
  [& forms]
  (cond-> (remove nil? (generate-fdef forms))
    (cljs-env? &env) clj->cljs))

#?(:clj (defn compiler-option [ks]
          (when cljs.env/*compiler*
            (get-in @cljs.env/*compiler* (into [:options :external-config] ks)))))

#?(:clj (defmacro dev-instrument! [f]
          (when (or (compiler-option [:fulcro.incubator.spec-helpers/instrument?]) (System/getProperty "instrument"))
            `(st/instrument ~f))))

#?(:clj (defmacro Defn [& forms]
          (let [sym   (first forms)
                nspc  (some-> (resolve sym) meta :ns str)
                fqsym (symbol nspc (name sym))]
            `(do
               (>defn ~@forms)
               (dev-instrument! (quote ~fqsym))
               ~sym))))

#?(:clj (defmacro Defn- [& forms]
          (let [sym   (first forms)
                nspc  (some-> (resolve sym) meta :ns str)
                fqsym (symbol nspc (name sym))]
            `(do
               (>defn- ~@forms)
               (dev-instrument! (quote ~fqsym))
               ~sym))))

