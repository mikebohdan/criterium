(ns criterium.time
  "Provides an augmented time macro for simple timing of expressions"
  (:refer-clojure :exclude [time])
  (:require [clojure.test.check
             [generators :as gen]
             [random :as random]
             [rose-tree :as rose]]
            [clojure.walk :as walk]
            [criterium
             [eval :as eval]
             [format :as format]
             [jvm :as jvm]
             [toolkit :as toolkit]]))

(def last-time* (volatile! nil))

(def metrics
  {:time              `toolkit/with-time
   :garbage-collector `toolkit/with-garbage-collector-stats
   :finalization      `toolkit/with-finalization-count
   :memory            `toolkit/with-memory
   :runtime-memory    `toolkit/with-runtime-memory
   :compilation       `toolkit/with-compilation-time
   :class-loader      `toolkit/with-class-loader-counts})

(defmulti format-metric (fn [metric val] metric))

(defmethod format-metric :time
  [_ val]
  (let [v (/ (:elapsed val) 1e9)]
    (format "%32s: %s\n" "Elapsed time" (format/format-value :time v))))

(defn- format-count-time [[k {c :count t :time}]]
  (format "%36s:  count %d  time %s\n" (name k) c (format/format-value :time t)))

(defmethod format-metric :garbage-collector
  [_ val]
  (format "%32s:\n%s" "Garbage collection"
       (apply str (map format-count-time val))))

(defmethod format-metric :finalization
  [_ val]
  (format "%32s: %d\n" "Pending finalisations" (:pending val)))

(defn- format-memory-metrics [[k vs]]
  (apply
    str
    (format "%36s:\n" (name k))
    (for [[mk v] vs]
      (format "%40s: %%s\n" (name mk) (format/format-value :memory v)))))

(defmethod format-metric :memory
  [_ val]
  (format "%32s:\n%s" "Memory"
          (apply str (map format-memory-metrics val))))

(defn- format-runtime-memory-metrics [[k v]]
  (format "%36s: %s\n" (name k) (format/format-value :memory v)))

(defmethod format-metric :runtime-memory
  [_ val]
  (format "%32s:\n%s" "Runtime Memory"
          (apply str (map format-runtime-memory-metrics val))))

(defmethod format-metric :compilation
  [_ val]
  (let [v (:compilation-time val)]
    (format "%32s: %s\n" "JIT Compilation time" (format/format-value :time v))))

(defn format-count [[k v]]
  (format "%36s: %d\n" (name k) v))

(defmethod format-metric :class-loader
  [_ val]
  (apply
    str
    (format "%32s:\n" "Classloader")
    (map format-count val)))

(defmethod format-metric :state
  [_ _]
  "")

(defmethod format-metric :expr-value
  [_ _]
  "")

(defmethod format-metric :num-evals
  [_ _]
  "")

(defn- wrap-for-metric [expr stat]
  `(~(metrics stat) ~expr))

(defn print-metrics [metrics]
  (doseq [[k v] metrics]
    (print (format-metric k v))))


;; (let [{:keys [f state-fn]} (measured-expr (inc 1))]
;;   (println :f f)
;;   (println :state-fn state-fn (state-fn))
;;   (f (state-fn)))


(defn sample-stats [samples]
  (let [sum (toolkit/total samples)
        avg (toolkit/divide sum (:num-evals sum))]
    avg))


(def DEFAULT-TIME-BUDGET-NS
  "Default time budget when no limit specified.
  100ms should be imperceptible."
  (* 100 toolkit/MILLISEC-NS))

(defn measure*
  "Evaluates measured and return metrics on its evaluation.

  The :metrics option accepts either :all, for all metrics, or a sequence of metric
  keyword selectors. Valid metrics
  are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
  :compilation, and :class-loader."
  [measured {:keys [limit-evals limit-time max-gc-attempts]
             :as   options}]
  (let [time-budget-ns (or limit-time DEFAULT-TIME-BUDGET-NS)
        eval-budget    (or limit-evals 10000)
        use-metrics    (let [use-metrics (:metrics options [])]
                         (if (= :all use-metrics)
                           (keys toolkit/measures)
                           use-metrics))
        pipeline       (toolkit/pipeline use-metrics)
        _              (toolkit/force-gc (or max-gc-attempts 3))
        vals           (toolkit/sample
                         measured
                         pipeline
                         {:time-budget-ns time-budget-ns
                          :eval-budget    eval-budget})
        report-stats?  (or limit-evals limit-time)]
    (if report-stats?
      (sample-stats vals)
      (last vals))))

(defmacro measure
  "Like time, but returns measurements rather than printing them."
  [expr & options]
  (let [options (apply hash-map options)]
    `(measure* (toolkit/measured-expr ~expr) ~options)))


(defn time*
  "Evaluates expr and prints the time it took.
  Return the value of expr.

  The timing info is available as a data structure by calling last-time.

  The :times option can be passed an integer specifying how many times to evaluate the
  expression.  The default is 1.  If a value greater than 1 is specified, then
  the value of the expression is not returned.

  The :metrics option accepts either :all, for all metrics, or a sequence of metric
  keyword selectors. Valid metrics
  are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
  :compilation, and :class-loader."
  [measured options]
  (let [deltas (measure* measured options)]
    (vreset! last-time* deltas)
    (print-metrics deltas)
    (:expr-value deltas)))

(defmacro time
  "Evaluates expr and prints the time it took.
  Return the value of expr.

  The timing info is available as a data structure by calling last-time.

  The :times option can be passed an integer specifying how many times to evaluate the
  expression.  The default is 1.  If a value greater than 1 is specified, then
  the value of the expression is not returned.

  The :metrics option accepts either :all, for all metrics, or a sequence of metric
  keyword selectors. Valid metrics
  are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
  :compilation, and :class-loader."
  [expr & options]
  (let [options          (apply hash-map options)
        ;; n                (:times options 1)
        ;; use-metrics      (let [use-metrics (:metrics options [:time])]
        ;;                    (if (= :all use-metrics) (keys metrics) use-metrics))
        ;; measured-fn-form (measured-expr expr)
        ;; f-sym            (gensym "f")
        ;; state-sym        (gensym "state")
        ]
    `(time* (toolkit/measured-expr ~expr) ~options)
    ;; `(let [measured-fn# ~measured-fn-form
    ;;        state-fn#    (:state-fn measured-fn#)
    ;;        ~state-sym   (state-fn#)
    ;;        ~f-sym       (:f measured-fn#)
    ;;        vals#        (toolkit/instrumented
    ;;                       ~(reduce wrap-for-metric
    ;;                                (if (= 1 n)
    ;;                                  `(toolkit/with-expr-value (~f-sym ~state-sym))
    ;;                                  `(toolkit/with-expr
    ;;                                     (eval/execute-n-times (~f-sym ~state-sym) ~n)))
    ;;                                use-metrics))
    ;;        deltas#      (toolkit/deltas (dissoc vals# :expr-value))]
    ;;    (vreset! last-time* deltas#)
    ;;    (print-metrics deltas#)
    ;;    (:expr-value vals#))
    ))

(time (inc 1))


(defn last-time
  "Return the data from the last time invocation."
  []
  @last-time*)


;; (time (inc 2) :metrics :all)
;; (time (inc 2))
;; (time (inc 2) :times 5000)
;; (time (Thread/sleep 1))
;; (time (Thread/sleep 1) :times 5000)

;; (clojure.pprint/pprint (last-time))

;; (defn time-vs-n
;;   []
;;   (let [ns [;; 1 10 100 1000
;;             10000 100000 1000000 10000000 100000000
;;             ]
;;         ts (for [n ns]
;;              (do
;;                (time (inc 2) :times n)
;;                (-> (last-time) :time :elapsed (/ n))))]
;;     (criterium.chart/view (criterium.chart/chart ns (vec ts)))))

;; ;; (time-vs-n)

;; (let [n 100]
;;   (time (inc 2) :times n)
;;   (-> (last-time) :time :elapsed ;; (/ (double n))
;;       ))


;; (* 1e9 (/ 1 2.2e9))


;; (bench/for [x (gen/choose 0 100000)]
;;   (timw (inc x)))




(defn ^:private apply-gen
  [function]
  (fn [args]
    (let [ret (apply function args)]
      {:result   ret
       :function function
       :args     args})))

(defn for-all*
  "A function version of `for-all`. Takes a sequence of N generators and a function of N
  args, and returns a measured function, which can be called with generated values, like
  with `for-all`.

  Example:

  (for-all* [gen/large-integer gen/large-integer]
            (fn [a b] (+ a b) a))"
  [args function]
  (gen/fmap
   (apply-gen function)
   (apply gen/tuple args)))

(defn- binding-vars
  [bindings]
  (map first (partition 2 bindings)))

(defn- binding-gens
  [bindings]
  (map second (partition 2 bindings)))

(defmacro for-all
  "Returns a timed function, which is the combination of some generators and an expression
  that should be measured for all generated values.

  `for-all` takes a `let`-style bindings vector, where the right-hand side of each
  binding is a generator.

  The body should be a timed form, with an expression of the generated values that will
  be measured.

  When there are multiple binding pairs, the earlier pairs are not
  visible to the later pairs.

  If there are multiple body expressions, all but the last one are
  executed for side effects, as with `do`.

  Example:

  (time
    (for-all [a gen/large-integer
              b gen/large-integer]
       (timed (+ a b))))"
  [bindings & body]
  `(for-all* ~(vec (binding-gens bindings))
             (fn [~@(binding-vars bindings)]
               ~@body)))

;; from c.t.check (private)
(defn- make-rng
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (jvm/timestamp)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn aggregate
  [num-evals measured & {:keys [seed max-size]
                         :or   {max-size 200}}]
  (toolkit/force-gc 10)
  (let [[created-seed rng] (make-rng seed)
        size-seq           (gen/make-size-range-seq max-size)
        ]
    (loop [so-far   0
           size-seq size-seq
           rstate   rng
           results []]
      (if (== so-far num-evals)
        {:results results
         :num-evals num-evals}
        (let [[size & rest-size-seq] size-seq
              [r1 r2]                (random/split rstate)
              result-map-rose        (gen/call-gen measured r1 size)
              result-map             (rose/root result-map-rose)
              result                 (:result result-map)
              so-far                 (inc so-far)]
          (recur so-far rest-size-seq r2 (conj results result)))))))

(defn report-results
  [{:keys [num-evals results]}]
  (println ;; "done" measured num-evals created-seed
    "Elapsed time: "
    (format/format-value
      :time
      (/ (reduce + (map (comp :elapsed :time) results)) (* num-evals 1e9)))))

(report-results
  (aggregate 10
             (for-all [i (gen/choose 0 1000000000000000000)]
               (measure (inc i)))))
