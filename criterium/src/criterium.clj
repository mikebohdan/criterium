(ns criterium
  "Criterium top level"
  (:refer-clojure :exclude [time])
  (:require [clojure.spec.alpha :as s]
            [criterium.analyze :as analyze]
            [criterium.config :as config]
            [criterium.domain :as domain]
            [criterium.measured :as measured]
            [criterium.output :as output]
            [criterium.pipeline :as pipeline]
            [criterium.report :as report]
            [criterium.sample-scheme :as sample-scheme]))

(defn measure
  "Evaluates measured and return measurement data."
  [measured config]
  (s/assert :criterium.config/config config)
  (let [pipeline (pipeline/pipeline (:pipeline config))
        sampled  (sample-scheme/sample
                  pipeline
                  measured
                  (:sample-scheme config))]
    (analyze/analyze
     sampled
     (pipeline/metrics (:pipeline config))
     (:analysis config))))

(def ^:no-doc last-time* (volatile! nil))

(defn last-time
  "Return the data from the last time invocation."
  []
  @last-time*)

(defn time-measured
  "Evaluates measured and prints the time it took.
  Return the value of calling the measured's wrapped function.

  The timing info is available as a data structure by calling last-time.

  The :times option can be passed an integer specifying how many times
  to evaluate the expression.  The default is 1.  If a value greater
  than 1 is specified, then the value of the expression is not returned.

  The :metrics option accepts either :all, for all metrics, or a
  sequence of metric keyword selectors. Valid metrics
  are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
  :compilation, and :class-loader."
  [measured config]
  (output/with-progress-reporting (:verbose config)
    (let [result (measure measured config)]
      (vreset! last-time* result)
      (report/report result config)
      ;; (if (:stats result)
      ;;   (do (report/print-stats (:stats result) config)
      ;;       (report/print-jvm-event-stats (:stats result) config)
      ;;       (report/print-final-gc-warnings (:stats result) config))
      ;;   (report/print-metrics result))
      (:expr-value result))))

(s/def ::limit-eval-count (s/or :empty? nil? :limit ::domain/eval-count))
(s/def ::limit-time-s (s/or :empty? nil? :limit (s/and number? pos?)))
(s/def ::pipeline (s/coll-of
                   (s/or ::pipeline/pipeline-fn-kw
                         ::pipeline/terminal-fn-kw)))
(s/def ::options (s/keys
                  :opt-un [::limit-eval-count ::limit-time-s ::pipeline]))

(defn config-map
  "Convert option arguments into a criterium option map.
  The options map specifies how criterium will execute."
  [options-map]
  (assert (s/valid? ::options options-map))
  (let [pipeline     (:pipeline options-map)
        terminator   (filterv pipeline/terminal-fn? pipeline)
        pipeline-fns (remove pipeline/terminal-fn? pipeline)
        scheme-type  (:sample-scheme options-map :full)
        histogram?   (:histogram options-map)]

    ;; (if (or (:histogram options) (:include-samples options))
    ;;   (assoc res :samples (:samples sampled))
    ;;   res)

    (when (> (count terminator) 1)
      (throw (ex-info
              "More than one terminal function specified in pipeline"
              {:terminators terminator})))
    (cond->
        (config/expand-options
         (cond-> (select-keys options-map [:verbose])
           (or
            (= scheme-type :full)
            histogram?)     (assoc :sample-scheme
                                   (config/full-sample-scheme
                                    options-map))
           (= scheme-type :one-shot) (assoc :sample-scheme
                                            (config/one-shot-sample-scheme
                                             options-map)
                                            :analysis []
                                            :report [{:report-type :metrics}])
           (seq pipeline-fns)        (assoc-in [:pipeline :stages]
                                               (vec pipeline-fns))
           (seq terminator)          (assoc-in [:pipeline :terminator]
                                               (first terminator))
           ;; (:histogram options-map) (update-in [:analysis] conj
           ;;                                     {:analysis-type :samples})

           ;; (:histogram options-map)  (assoc-in
           ;;                            [:output :histogram] true)
           ))
      histogram? (update-in [:analysis] conj
                            {:analysis-type :samples})
      histogram? (update-in [:report] conj
                            {:report-type :histogram}))))

(defmacro time
  "Evaluates expr and prints the time it took.
  Return the value of expr.

  The timing info is available as a data structure by calling last-time.

  The :times option can be passed an integer specifying how many times
  to evaluate the expression.  The default is 1.  If a value greater
  than 1 is specified, then the value of the expression is not returned.

  The :metrics option accepts either :all, for all metrics, or a
  sequence of metric keyword selectors. Valid metrics
  are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
  :compilation, and :class-loader."
  [expr & options]
  `(time-measured
    (measured/expr ~expr)
    (config-map
     (hash-map ~@options))))
