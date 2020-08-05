(ns criterium
  "Criterium top level"
  (:refer-clojure :exclude [time])
  (:require [criterium
             [measure :as measure]
             [measured :as measured]
             [report :as report]]))


(def last-time* (volatile! nil))

(defn last-time
  "Return the data from the last time invocation."
  []
  @last-time*)


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
  (let [result (measure/measure measured options)]
    (vreset! last-time* result)
    (if (:stats result)
      (report/print-stats result options)
      (report/print-metrics result))
    (:expr-value result)))

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
  (let [options (apply hash-map options)]
    `(time* (measured/expr ~expr) ~options)))


;; (time 1)
;; (time 1 :sample-mode :full)
;; (time 1 :sample-mode :one-shot)
