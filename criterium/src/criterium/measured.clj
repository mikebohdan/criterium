(ns criterium.measured
  "Defines the Measured type."
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [criterium.domain :as domain]
            [criterium.jvm :as jvm]
            [criterium.util :as util])
  (:import [org.openjdk.jmh.infra Blackhole]))

;;; Measured type

(defrecord Measured
           [^clojure.lang.IFn state-fn
            ^clojure.lang.IFn f
            expr-fn])

(defn measured?
  "Predicate for x being a Measured"
  [x]
  (instance? Measured x))

(defn measured
  "Return a Measured for a function that can be benchmarked.

  The Measured is the basic unit of measurement. A Measured consists of
  a state generation funtion and a function to be measured.  The
  function to be measured takes the result of calling the state function
  as an argument, ie. `(f (state-fn))`, and returns an `[elapsed-time
  expr-value]` tuple.

  The state function is used to prevent constant folding for constant
  inputs.

  The eval-count allows usage where the function is a wrapper that
  evaluates the subject expression multiple times.

  expr-fn, if specified, returns a symbolic representation of the measured,
  for inspection purposes (unused internally).
  "
  ^Measured
  [state-fn
   f
   & [expr-fn]]
  (->Measured state-fn f expr-fn))

(defn ^:no-doc symbolic
  "Return a symbolic representation of the measured.
  Provides a way to introspect how the measured will be executed."
  [measured]
  (when-let [expr-fn (:expr-fn measured)]
    (expr-fn)))

(defn invoke
  "Invoke the given Measured.

  Calls the Measured's function with the result of calling the
  Measured's state function."
  ([^Measured measured]
   (invoke measured 1))
  ([^Measured measured eval-count]
   ((.f measured) ((:state-fn measured)) eval-count)))

;;; Blackhole

(def ^{:tag 'org.openjdk.jmh.infra.Blackhole :no-doc true} blackhole
  (Blackhole.
   "Today's password is swordfish. I understand instantiating Blackholes directly is dangerous."))

(defn ^:no-doc evaporate []
  (.evaporate
   blackhole
   "Yes, I am Stephen Hawking, and know a thing or two about black holes."))

;;; Build a Measured from an expression

(defn- s-expression?
  "Predicate for expr being an S-expression."
  [expr]
  (or (list? expr) (instance? clojure.lang.Cons expr)))

(defrecord FnCallExpr
    ;; a representation of an s-expression
           [op                                 ; the operand
            arg-syms                           ; arguments as symbols
            arg-vals                           ; the symbolic value of of the arg-syms
            metamap                            ; metadata on the FnCallExpr
            ])

(defn- fn-call-expr?
  "Predicate for x being an instance of a FnCallExpr"
  [x]
  (instance? FnCallExpr x))

(defn- gen-arg-sym
  "Generate a symbol for an argument."
  []
  (gensym "arg"))

(defn ^:no-doc form-print
  "Return a symbolic expression for the argument."
  [x]
  (cond
    (symbol? x) x

    (fn-call-expr? x)
    (with-meta
      `(~(:op x) ~@(mapv form-print (:arg-syms x)))
      (:metamap x))))

(defn ^:no-doc factor-form
  "Factor form, extracting constant expressions."
  [form]
  (let [subj  (first form)
        tform (fn [x]
                (if (and (s-expression? x) (= subj (first x)))
                  (reduce
                   (fn [res arg]
                     (if (fn-call-expr? arg)
                       (-> res
                           (update :arg-syms conj arg)
                           (update :arg-vals merge (:arg-vals arg)))
                       (let [arg-sym (gen-arg-sym)]
                         (-> res
                             (update :arg-syms conj arg-sym)
                             (update :arg-vals assoc arg-sym arg)))))
                   (->FnCallExpr
                    (first x)
                    []
                    {}
                    (meta x))
                   (rest x))
                  x))
        res   (util/postwalk
               tform
               form)]
    {:expr     (form-print res)
     :arg-vals (:arg-vals res)}))

(defn ^:no-doc factor-const [expr]
  (let [arg-sym (gen-arg-sym)]
    {:expr     arg-sym
     :arg-vals {arg-sym expr}}))

(defn ^:no-doc factor-expr [expr]
  (if (s-expression? expr)
    (factor-form expr)
    (factor-const expr)))

(defn ^:no-doc cast-fn
  "Return a cast function givent a tag."
  [tag]
  (when (and (symbol? tag)
             (#{'long 'int 'double 'float} tag))
    tag))

(defn ^:no-doc binding-with-hint-or-cast
  "Return a binding pair to type hint or cast values."
  [arg-sym arg-meta]
  (let [tag (:tag arg-meta)]
    (if-let [f (cast-fn tag)]
      [arg-sym (list f arg-sym)]
      [(with-meta arg-sym arg-meta) arg-sym])))

(defn ^:internal ^:no-doc measured-expr-fn
  "Construct a function expression to measure the given expr, with the given args."
  [arg-syms expr & [{:keys [arg-metas]}]]
  (let [blackhole-sym  (with-meta (gensym "blachole")
                         {:tag 'org.openjdk.jmh.infra.Blackhole})
        eval-count-sym  (gensym "eval-count")]
    `(fn ~'measured [~arg-syms ~(with-meta eval-count-sym {:tag 'long})]
       (let [~blackhole-sym blackhole ; hoist cast lookup out of loop
             ~@(mapcat binding-with-hint-or-cast arg-syms arg-metas)
             ;; primitive loop coounter.  Decrement since we evaluate
             ;; once outside the loop.
             n#             (long (unchecked-dec ~eval-count-sym))
             start#         (criterium.jvm/timestamp)
             val#           ~expr]      ; evaluate once to get a return value
         (loop [i# n#]
           (when (pos? i#)
             ;; don't use a local inside the loop, to avoid locals clearing
             (.consume ~blackhole-sym ~expr)
             (recur (long (unchecked-dec i#)))))
         (let [finish# (criterium.jvm/timestamp)]
           (evaporate)
           [(unchecked-subtract finish# start#) val#])))))

(defn ^:no-doc merge-metas
  "Merge two sequences of maps.
  Sequences may be of differing lengths.  The returned length is the
  largest of the two input lengths."
  [m1 m2]
  (let [l1 (count m1)
        l2 (count m2)]
    (into (mapv merge m1 m2)
          (if (>= l1 l2)
            (drop l2 m1)
            (drop l1 m2)))))

(def TYPE-NAME-CONVERSIONS
  ;; converting is good for measureds that use these values directly
  ;; but causes wrapping if the values are returned from the measured.
  {'java.lang.Long    'long
   'java.lang.Integer 'int
   'java.lang.Double  'double
   'java.lang.Float   'float})

(defn ^:no-doc type-name-conversion [t]
  (TYPE-NAME-CONVERSIONS t t))

(defn ^:no-doc tag-meta [^Class t]
  (when t
    (let [type-name (-> (.getCanonicalName ^Class t)
                        symbol
                        type-name-conversion)]
      {:tag type-name})))

(defn ^:no-doc capture-arg-types
  "Use eval to get types of the arg expressions.
  Return a sequence of metadata maps with :tag tupe hints."
  [arg-exprs]
  (let [types (mapv (comp type eval) arg-exprs)]
    (mapv tag-meta types)))

(defn- measured-expr*
  "Return a measured function for the given expression.

  The arguments are converted into a vector, which is used as an
  argument to the a function that wraps the expression.

  Any expr that is not a List is treated as a constant.  This is mainly
  for internal benchmarking."
  [expr & [options]]
  (let [{:keys [expr arg-vals] :as _f} (factor-expr expr)
        arg-metas (capture-arg-types (vals arg-vals))
        options (update options :arg-metas merge-metas arg-metas)]
    `(measured
      (fn ~'measured-state [] ~(vec (vals arg-vals)))
      ~(measured-expr-fn
        (vec (keys arg-vals))
        expr
        options)
      (fn ~'measured-expr []
        ~(list 'quote
               `(do (let [~@arg-vals]
                      (time ~expr))))))))

(defmacro expr
  "Return a Measured for the given expression.

  The expression is wrapped in a function.

  When the expression is a list form, it is treated as a function call.
  The function arguments are treated as constant expressions and are
  hoisted into a state function.  The result of the state function is a
  vector that is passed to the function wrapper as a vector and
  destructured."
  [expr & [options]]
  (measured-expr* expr options))

;; (defn batch
;;   "Wrap `measured` to run `eval-count` times.

;;   Inside the loop, the return value of each execution is put into a sink,
;;   The sink function can be customised by passing the `:sink-fn` options.

;;   Return a Measured."
;;   [m ^long eval-count]
;;   {:pre [(>= eval-count 1)]}
;;   (assoc m :eval-count eval-count))

(s/def ::state-fn (s/fspec
                   :args (s/cat)
                   :ret any?))

(s/def ::measured-tuple (s/tuple ::domain/elapsed-time-ns any?))

(s/def ::measure-fn (s/fspec
                     :args (s/cat :state any? :count ::domain/eval-count)
                     :ret ::measured-tuple))

(s/def ::expr-fn (s/or :empty nil?
                       :fn (s/fspec
                            :args (s/cat)
                            :ret sequential?)))

(s/def ::measured
  (s/with-gen
    (s/and measured?
           (comp ifn? :state-fn)
           (comp ifn? :f)
           (comp (some-fn nil? ifn?) :expr-fn))
    #(sgen/fmap
      (fn [[s t u]] (measured s t u))
      (sgen/tuple
       (s/gen ::state-fn)
       (s/gen ::measured-fn)
       (s/gen ::expr-fn)))))

(s/fdef measured
  :args (s/cat :state-fn ::state-fn
               :measured-fn ::measured-fn
               :expr-fn (s/? ::expr-fn))
  :ret ::measured)

(s/fdef invoke
  :args (s/cat :measured ::measured :eval-count ::domain/eval-count)
  :ret ::measured-tuple)

(s/fdef expr
  :args (s/cat :expr any?)
  :ret ::measured)
