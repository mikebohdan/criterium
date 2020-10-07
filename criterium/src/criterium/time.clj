(ns criterium.time
  "Provides an augmented time macro for simple timing of expressions"
  (:refer-clojure :exclude [time])
  ;; (:require [criterium
  ;;            [format :as format]
  ;;            [jvm :as jvm]
  ;;            [measured :as measured]
  ;;            [stats :as stats]
  ;;            [toolkit :as toolkit]
  ;;            [well :as well]])
  )

(def last-time* (volatile! nil))

;; (def metrics
;;   {:time              `toolkit/with-time
;;    :garbage-collector `toolkit/with-garbage-collector-stats
;;    :finalization      `toolkit/with-finalization-count
;;    :memory            `toolkit/with-memory
;;    :runtime-memory    `toolkit/with-runtime-memory
;;    :compilation       `toolkit/with-compilation-time
;;    :class-loader      `toolkit/with-class-loader-counts})

;; (defmulti format-metric (fn [metric val] metric))

;; (defmethod format-metric :time
;;   [_ val]
;;   (let [v (/ val 1e9)]
;;     (format "%32s: %s\n" "Elapsed time" (format/format-value :time v))))

;; (defn- format-count-time [[k {c :count t :time}]]
;;   (format "%36s:  count %d  time %s\n" (name k) c (format/format-value :time t)))

;; (defmethod format-metric :garbage-collector
;;   [_ val]
;;   (format "%32s:\n%s" "Garbage collection"
;;        (apply str (map format-count-time val))))

;; (defmethod format-metric :finalization
;;   [_ val]
;;   (format "%32s: %d\n" "Pending finalisations" (:pending val)))

;; (defn- format-memory-metrics [[k vs]]
;;   (apply
;;     str
;;     (format "%36s:\n" (name k))
;;     (for [[mk v] vs]
;;       (format "%40s: %s\n" (name mk) (format/format-value :memory v)))))

;; (defmethod format-metric :memory
;;   [_ val]
;;   (format "%32s:\n%s" "Memory"
;;           (apply str (map format-memory-metrics val))))

;; (defn- format-runtime-memory-metrics [[k v]]
;;   (format "%36s: %s\n" (name k) (format/format-value :memory v)))

;; (defmethod format-metric :runtime-memory
;;   [_ val]
;;   (format "%32s:\n%s" "Runtime Memory"
;;           (apply str (map format-runtime-memory-metrics val))))

;; (defmethod format-metric :compilation
;;   [_ val]
;;   (let [v (:compilation-time val)]
;;     (format "%32s: %s\n" "JIT Compilation time" (format/format-value :time v))))

;; (defn format-count [[k v]]
;;   (format "%36s: %d\n" (name k) v))

;; (defmethod format-metric :class-loader
;;   [_ val]
;;   (apply
;;     str
;;     (format "%32s:\n" "Classloader")
;;     (map format-count val)))

;; (defmethod format-metric :state
;;   [_ _]
;;   "")

;; (defmethod format-metric :expr-value
;;   [_ _]
;;   "")

;; (defmethod format-metric :num-evals
;;   [_ _]
;;   "")

;; (def metric-paths
;;   {:time [[:time]]
;;    :memory [[:memory :total :used]
;;             [:memory :heap :used]]
;;    :class-loader [[:class-loader :loaded-count]
;;                   [:class-loader :unloaded-count]]
;;    :compilation-time [[:compilation-time :compilation-time]]
;;    :runtime-memory [[:runtime-memory :max]
;;                     [:runtime-memory :free]
;;                     [:runtime-memory :total]]
;;    :finalization-count [:finalization-count :pending]
;;    :garbage-collector [[:garbage-collector :total :time]
;;                        [:garbage-collector :total :count]]})

;; (defn path-accessor
;;   [path]
;;   (reduce comp (reverse path)))

;; (def metric-format
;;   {[:time]                               {:dimension :ns :label "Elapsed Time"}
;;    [:memory :total :used]                {:dimension :memory
;;                                           :label     "Total Used Memory"}
;;    [:memory :heap :used]                 {:dimension :memory
;;                                           :label     "Total Heap Memory"}
;;    [:class-loader :loaded-count]         {:dimension :count :label "Classes Loaded"}
;;    [:class-loader :unloaded-count]       {:dimension :count :label "Classes Unloaded"}
;;    [:compilation-time :compilation-time] {:dimension :ns :label "Compilation Time"}
;;    [:runtime-memory :max]                {:dimension :memory
;;                                           :label     "Max Memory (runtime)"}
;;    [:runtime-memory :free]               {:dimension :memory
;;                                           :label     "Free Memory (runtime)"}
;;    [:runtime-memory :total]              {:dimension :memory
;;                                           :label     "Total Memory (runtime)"}
;;    [:finalization-count :pending]        {:dimension :count
;;                                           :label     "Finalizations Pending"}
;;    [:garbage-collector :total :time]     {:dimension :ns :label "GC Time"}
;;    [:garbage-collector :total :count]    {:dimension :count :label "GC count"}})

;; ;; (defn- wrap-for-metric [expr stat]
;; ;;   `(~(metrics stat) ~expr))

;; (defn print-stat [path {:keys [mean variance] :as stat}]
;;   (let [{:keys [dimension label]} (metric-format path)
;;         [scale units] (format/scale dimension (first mean))]
;;     (println
;;       (format "%36s: %.3g ± %.3g %s"
;;               label
;;               (* scale (first mean))
;;               (* scale 3 (Math/sqrt (first variance)))
;;               units))))

;; (defn view-histogram [{:keys [samples batch-size stats] :as result} path]
;;   (let [stat (get-in stats path)
;;         mean (first (:mean stat))
;;         {:keys [dimension label]} (metric-format path)
;;         [scale units] (format/scale dimension mean)
;;         vs (->>
;;              samples
;;              (mapv (path-accessor path))
;;              (mapv #(/ % (double batch-size)))
;;              (mapv #(* % scale)))
;;         chart-options {:title label
;;                        :value-label (str "value [" units"]")}]
;;     (criterium.chart/view
;;       (criterium.chart/histogram
;;         vs
;;         chart-options))))

;; (defn print-stats [result {:keys [histogram] :as options}]
;;   (doseq [metric (:metrics result)]
;;     (doseq [path (metric-paths metric)]
;;       (let [stat (get-in (:stats result) path)]
;;         (print-stat path stat)
;;         (when histogram
;;           (view-histogram result path)))))
;;   )

;; (defn print-metrics [metrics]
;;   (doseq [[k v] metrics]
;;     (print (format-metric k v))))


;; ;; (let [{:keys [f state-fn]} (measured-expr (inc 1))]
;; ;;   (println :f f)
;; ;;   (println :state-fn state-fn (state-fn))
;; ;;   (f (state-fn)))


;; (defn stats-for [path batch-size samples opts]
;;   (let [vs            (mapv (path-accessor path) samples)
;;         tail-quantile (:tail-quantile opts 0.025)
;;          ;; _ (println "path" path "vs" vs)
;;         stats         (stats/bootstrap-bca
;;                         (mapv double vs)
;;                         (juxt
;;                           stats/mean
;;                           stats/variance
;;                           (partial stats/quantile 0.5)
;;                           (partial stats/quantile tail-quantile)
;;                           (partial stats/quantile (- 1.0 tail-quantile)))
;;                         (:bootstrap-size opts 100)
;;                         [0.5 tail-quantile (- 1.0 tail-quantile)]
;;                         well/well-rng-1024a)
;;         ks [:mean :variance :median :0.025 :0.975 ]
;;         sqr-batch-size (stats/sqr batch-size)
;;         scale-1 (fn [[p [l u]]]
;;                    [(/ p batch-size)
;;                     [(/ l batch-size) (/ u batch-size)]])
;;         scale-2 (fn [[p [l u]]]
;;                    [(/ p sqr-batch-size)
;;                     [(/ l sqr-batch-size) (/ u sqr-batch-size)]]) ;; TODO FIXME
;;         scale-fns {:mean scale-1
;;                    :variance scale-2
;;                    :median scale-1
;;                    :0.025 scale-1
;;                    :0.975 scale-1}

;;         stats (zipmap ks stats)
;;         stats (zipmap ks
;;                       (mapv
;;                         (fn [k]
;;                           ((scale-fns k) (k stats)))
;;                         ks))]
;;     stats))

;; (defn sample-stats [metrics batch-size samples {:keys [return-samples] :as opts}]
;;   ;; (clojure.pprint/pprint samples)
;;   (let [sum           (toolkit/total samples)
;;         num-evals     (:num-evals sum)
;;         avg           (toolkit/divide sum num-evals)
;;         paths (mapcat metric-paths metrics)
;;         stats (reduce
;;                 (fn [res path]
;;                   (assoc-in res path (stats-for path batch-size samples opts)))
;;                 {}
;;                 paths)

;;         ;; times         (mapv toolkit/elapsed-time samples)
;;         ;; tail-quantile (:tail-quantile opts 0.025)
;;         ;; stats         (stats/bootstrap-bca
;;         ;;                 (mapv double times)
;;         ;;                 (juxt
;;         ;;                   stats/mean
;;         ;;                   stats/variance
;;         ;;                   (partial stats/quantile 0.5)
;;         ;;                   (partial stats/quantile tail-quantile)
;;         ;;                   (partial stats/quantile (- 1.0 tail-quantile)))
;;         ;;                 (:bootstrap-size opts 100)
;;         ;;                 [0.5 tail-quantile (- 1.0 tail-quantile)]
;;         ;;                 well/well-rng-1024a)

;;         ;; ks [:mean :variance :median :0.025 :0.975 ]
;;         ;; stats (zipmap ks stats)
;;         ;; sqr-batch-size (stats/sqr batch-size)
;;         ;; scale-1 (fn [[p [l u]]]
;;         ;;            [(/ p batch-size)
;;         ;;             [(/ l batch-size) (/ u batch-size)]])
;;         ;; scale-2 (fn [[p [l u]]]
;;         ;;            [(/ p sqr-batch-size)
;;         ;;             [(/ l sqr-batch-size) (/ u sqr-batch-size)]]) ;; TODO FIXME
;;         ;; scale-fns {:mean scale-1
;;         ;;            :variance scale-2
;;         ;;            :median scale-1
;;         ;;            :0.025 scale-1
;;         ;;            :0.975 scale-1}
;;         ]

;;     (cond->
;;         {:num-evals   num-evals
;;          :avg         avg
;;          :stats       stats
;;          ;; (zipmap ks
;;          ;;                      (mapv
;;          ;;                        (fn [k]
;;          ;;                          ((scale-fns k) (k stats)))
;;          ;;                        ks))
;;          :num-samples (count samples)
;;          :batch-size batch-size
;;          :metrics metrics}
;;       return-samples (assoc :samples samples))))


;; (defn- pipeline-args [options]
;;   (let [use-metrics    (let [use-metrics (:metrics options [])]
;;                          (if (= :all use-metrics)
;;                            (keys toolkit/measures)
;;                            use-metrics))
;;         ;; pipeline-options (if ((set use-metrics) :with-time)
;;         ;;                    {:terminal-fn toolkit/with-expr-value})
;;         pipeline-options {}
;;         ]
;;     [(vec (remove #{:with-time} use-metrics))
;;      pipeline-options]))

;; (defn- pipeline-for-options [options]
;;   (let [use-metrics    (let [use-metrics (:metrics options [])]
;;                          (if (= :all use-metrics)
;;                            (keys toolkit/measures)
;;                            use-metrics))
;;         pipeline-options {};; (if ((set use-metrics) :with-expr-value)
;;                            ;; {:terminal-fn toolkit/with-expr-value})
;;         ]
;;     (toolkit/pipeline
;;       (remove #{:with-time} use-metrics)
;;       pipeline-options)))

;; (defn measure-stats
;;   "Evaluates measured and return metrics on its evaluation.

;;   The :metrics option accepts either :all, for all metrics, or a sequence of metric
;;   keyword selectors. Valid metrics
;;   are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
;;   :compilation, and :class-loader."
;;   [measured
;;    {:keys [limit-evals limit-time max-gc-attempts warmup-period warmup-fraction]
;;     :as   options}]
;;   ;; Start by running GC until it has nothing to do.
;;   (toolkit/force-gc (or max-gc-attempts 3))
;;   (let [;; primitive estimate
;;         t0                       (toolkit/first-estimate measured)
;;         ;; _ (println "t0" t0)

;;         ;; budgets
;;         time-budget-ns           (if limit-time
;;                                    (long (* limit-time toolkit/SEC-NS))
;;                                    (if limit-evals
;;                                      (* limit-evals t0)  ; should be an upper bound
;;                                      (max DEFAULT-TIME-BUDGET-NS
;;                                           (* t0 100)
;;                                           ;; (* 5 toolkit/SEC-NS)
;;                                           )))
;;         eval-budget              (or limit-evals
;;                                      (* 3 (long (quot time-budget-ns t0))))
;;         warmup-period-ns         (some-> warmup-period (* toolkit/SEC-NS))

;;         ;; _ (println {:time-budget-ns time-budget-ns
;;         ;;             :eval-budget eval-budget})

;;         ;; warmup - non batched function
;;         {:keys [num-evals time-ns samples measured-batch] :as warmup-result}
;;         (toolkit/warmup
;;           measured
;;           t0
;;           (merge
;;             (select-keys options [:sink-fn])
;;             {:time-budget-ns   time-budget-ns
;;              :eval-budget      eval-budget
;;              :warmup-period-ns warmup-period-ns}))
;;         time-budget-ns (- time-budget-ns time-ns)
;;         eval-budget (- eval-budget num-evals)
;;         t1 (/ (reduce + (map toolkit/elapsed-time samples))
;;               (reduce + (map :num-evals samples)))
;;         ;; _ (println "warmup-result" warmup-result)
;;         ;; _ (println "t1" t1 "batch-size" (:eval-count measured-batch))
;;         ;; _ (println {:time-budget-ns time-budget-ns
;;         ;;             :eval-budget eval-budget})

;;         _              (toolkit/force-gc (or max-gc-attempts 3))

;;         {:keys [num-evals time-ns samples ^Measured measured-batch]
;;          :as warmup-result}
;;         (toolkit/warmup
;;           measured
;;           t1
;;           {:time-budget-ns   time-budget-ns
;;            :eval-budget      eval-budget
;;            :warmup-period-ns warmup-period-ns})
;;         time-budget-ns (- time-budget-ns time-ns)
;;         eval-budget (- eval-budget num-evals)
;;         t2 (/ time-ns num-evals)
;;         ;; _ (println "warmup-result" warmup-result)
;;         ;; _ (println "t2" t2 "batch-size" (:eval-count measured-batch))
;;         ;; _ (println {:time-budget-ns time-budget-ns
;;         ;;             :eval-budget eval-budget})

;;         ;; ;;; batch size estimate
;;         ;; estimate       (toolkit/estimate-execution-time
;;         ;;                  measured
;;         ;;                  {:time-budget-ns (quot time-budget-ns 4)
;;         ;;                   :eval-budget    (quot eval-budget 4)})
;;         ;; _              (println "estimate" estimate)
;;         ;; time-budget-ns (- time-budget-ns (:sum estimate))
;;         ;; eval-budget    (- eval-budget (:num-evals estimate))

;;         _              (toolkit/force-gc (or max-gc-attempts 3))

;;         [use-metrics pl-options] (pipeline-args options)
;;         pipeline                 (toolkit/pipeline
;;                                    use-metrics
;;                                    options)
;;         vals           (if (= toolkit/with-time
;;                               (:terminal-fn pl-options))
;;                          (toolkit/sample-no-time
;;                            measured-batch
;;                            pipeline
;;                            {:eval-budget eval-budget})
;;                          (toolkit/sample
;;                            measured-batch
;;                            pipeline
;;                            {:time-budget-ns time-budget-ns
;;                             :eval-budget    eval-budget}))]

;;     (sample-stats
;;       (conj use-metrics :time)
;;       (:eval-count measured-batch)
;;       vals
;;       options)))

;; (defn measure-point-value
;;   "Evaluates measured and return metrics on its evaluation.

;;   The :metrics option accepts either :all, for all metrics, or a sequence of metric
;;   keyword selectors. Valid metrics
;;   are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
;;   :compilation, and :class-loader."
;;   [measured {:keys [max-gc-attempts metrics terminsl-fn]
;;              :as   options}]
;;   (let [time-budget-ns DEFAULT-TIME-BUDGET-NS
;;         eval-budget    10000
;;         [use-metrics pl-options] (pipeline-args options)
;;         pipeline                 (toolkit/pipeline
;;                                    use-metrics
;;                                    options)
;;         ;; let functions allocate on initial invocation
;;         _              (measured/invoke measured)
;;         _              (toolkit/force-gc (or max-gc-attempts 3))
;;         vals           (toolkit/sample
;;                            measured
;;                            pipeline
;;                            {:time-budget-ns time-budget-ns
;;                             :eval-budget    eval-budget})]
;;     (last vals)))


;; (defn measure*
;;   "Evaluates measured and return metrics on its evaluation.

;;   The :metrics option accepts either :all, for all metrics, or a sequence of metric
;;   keyword selectors. Valid metrics
;;   are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
;;   :compilation, and :class-loader."
;;   [measured {:keys [batch-size limit-evals limit-time max-gc-attempts metrics stats
;;                     sink-fn]
;;              :as   options}]
;;   ;; let [measured (if batch-size
;;   ;;                 (toolkit/measured-batch measured batch-size)
;;   ;;                 measured)]
;;   (if (or limit-evals limit-time stats)
;;     (measure-stats measured options)
;;     (measure-point-value measured options)))

;; (defmacro measure
;;   "Like time, but returns measurements rather than printing them."
;;   [expr & options]
;;   (let [options (apply hash-map options)]
;;     `(measure* (measured/expr ~expr) ~options)))


;; (defn time*
;;   "Evaluates expr and prints the time it took.
;;   Return the value of expr.

;;   The timing info is available as a data structure by calling last-time.

;;   The :times option can be passed an integer specifying how many times to evaluate the
;;   expression.  The default is 1.  If a value greater than 1 is specified, then
;;   the value of the expression is not returned.

;;   The :metrics option accepts either :all, for all metrics, or a sequence of metric
;;   keyword selectors. Valid metrics
;;   are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
;;   :compilation, and :class-loader."
;;   [measured options]
;;   (let [result (measure* measured (assoc options :return-samples true))]
;;     (vreset! last-time* result)
;;     (if (:num-samples result)
;;       (print-stats result options)
;;       (print-metrics result))
;;     (:expr-value result)))

;; (defmacro time
;;   "Evaluates expr and prints the time it took.
;;   Return the value of expr.

;;   The timing info is available as a data structure by calling last-time.

;;   The :times option can be passed an integer specifying how many times to evaluate the
;;   expression.  The default is 1.  If a value greater than 1 is specified, then
;;   the value of the expression is not returned.

;;   The :metrics option accepts either :all, for all metrics, or a sequence of metric
;;   keyword selectors. Valid metrics
;;   are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
;;   :compilation, and :class-loader."
;;   [expr & options]
;;   (let [options          (apply hash-map options)
;;         ;; n                (:times options 1)
;;         ;; use-metrics      (let [use-metrics (:metrics options [:time])]
;;         ;;                    (if (= :all use-metrics) (keys metrics) use-metrics))
;;         ;; measured-fn-form (measured-expr expr)
;;         ;; f-sym            (gensym "f")
;;         ;; state-sym        (gensym "state")
;;         ]
;;     `(time* (measured/expr ~expr) ~options)
;;     ;; `(let [measured-fn# ~measured-fn-form
;;     ;;        state-fn#    (:state-fn measured-fn#)
;;     ;;        ~state-sym   (state-fn#)
;;     ;;        ~f-sym       (:f measured-fn#)
;;     ;;        vals#        (toolkit/instrumented
;;     ;;                       ~(reduce wrap-for-metric
;;     ;;                                (if (= 1 n)
;;     ;;                                  `(toolkit/with-expr-value (~f-sym ~state-sym))
;;     ;;                                  `(toolkit/with-expr
;;     ;;                                     (eval/execute-n-times (~f-sym ~state-sym) ~n)))
;;     ;;                                use-metrics))
;;     ;;        deltas#      (toolkit/deltas (dissoc vals# :expr-value))]
;;     ;;    (vreset! last-time* deltas#)
;;     ;;    (print-metrics deltas#)
;;     ;;    (:expr-value vals#))
;;     ))

;; ;; (time (inc 1))


;; (defn last-time
;;   "Return the data from the last time invocation."
;;   []
;;   @last-time*)


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

;; (measure 1 :stats true)


;; criterium.time> (dissoc (measure (Thread/sleep 10) :stats true) :samples)
;; {:time-budget-ns 100000000, :eval-budget 100000000/3928951}
;; t1 11215449 batch-size 1
;; {:time-budget-ns 55138201, :eval-budget 84284196/3928951}
;; t2 12018124 batch-size 1
;; {:time-budget-ns -4952422, :eval-budget 64639441/3928951}
;; []
;; Execution error (ArithmeticException) at criterium.stats/mean (stats.clj:37).
;; Divide by zero


;; (dissoc (measure (Thread/sleep 1000) :stats true) :samples)
;; Execution error (ArithmeticException) at criterium.stats/mean (stats.clj:37).
;; Divide by zero

#_(comment

  (time 1)

  (time (Thread/sleep 10))

  (time 1 :limit-time 1)


  (time (do
          (range 100000)
          (range 10))
        :metrics
        [:memory
         :runtime-memory
         :finalization-count])

  (time (repeatedly 100 #(Object.))
        :metrics
        [:memory
         :class-loader
         :compilation-time
         :runtime-memory
         :finalization-count
         :garbage-collector])

  )
