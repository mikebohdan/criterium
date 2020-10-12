(ns criterium
  "Criterium top level"
  (:refer-clojure :exclude [time])
  (:require [criterium
             [measure :as measure]
             [measured :as measured]
             [output :as output]
             [pipeline :as pipeline]
             [report :as report]]))

(def ^:no-doc last-time* (volatile! nil))

(defn last-time
  "Return the data from the last time invocation."
  []
  @last-time*)

(defn- options-map
  "Convert option arguments into a criterium option map.
  The options map specifies how criterium will execute."
  [option-args]
  (let [options-map  (apply hash-map option-args)
        pipeline     (:pipeline options-map)
        terminator   (filterv pipeline/terminal-fn? pipeline)
        pipeline-fns (remove pipeline/terminal-fn? pipeline)]
    (when (> (count pipeline) 1)
      (throw (ex-info
              "More than one terminal function specified in pipeline"
              {:terminators terminator})))
    (measure/expand-options
     (cond-> options-map
       true               (dissoc :pipeline :terminator)
       (seq pipeline-fns) (assoc-in [:pipeline :stages] (vec pipeline-fns))
       (seq terminator)   (assoc-in [:pipeline :terminator] (first terminator))))))

(defn time-measured
  "Evaluates measured and prints the time it took.
  Return the value of calling the measured's wrapped function.

  The timing info is available as a data structure by calling last-time.

  The :times option can be passed an integer specifying how many times to evaluate the
  expression.  The default is 1.  If a value greater than 1 is specified, then
  the value of the expression is not returned.

  The :metrics option accepts either :all, for all metrics, or a sequence of metric
  keyword selectors. Valid metrics
  are :time, :garbage-collector, :finalization, :memory, :runtime-memory,
  :compilation, and :class-loader."
  [measured & options]
  (let [options (options-map options)]
    (output/with-progress-reporting (:verbose options)
      (let [result (measure/measure measured options)]
        (vreset! last-time* result)
        (if (:stats result)
          (do (report/print-stats (:stats result) options)
              (report/print-jvm-event-stats (:stats result) options)
              (report/print-final-gc-warnings (:stats result) options))
          (report/print-metrics result))
        (:expr-value result)))))

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
  `(time-measured (measured/expr ~expr) ~@options))
