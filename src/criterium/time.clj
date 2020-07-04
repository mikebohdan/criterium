(ns criterium.time
  "Provides an augmented time macro for simple timing of expressions"
  (:refer-clojure :exclude [time])
  (:require [criterium.toolkit :as toolkit]
            [criterium.core :as criterium]))

(def last-time* (volatile! nil))

(def metrics
  {:time              'toolkit/with-time
   :garbage-collector 'toolkit/with-garbage-collector-stats
   :finalization      'toolkit/with-finalization-count
   :memory            'toolkit/with-memory
   :runtime-memory    'toolkit/with-runtime-memory
   :compilation       'toolkit/with-compilation-time
   :class-loader      'toolkit/with-class-loader-counts})

(defmulti format-metric (fn [metric val] metric))

(defmethod format-metric :time
  [_ val]
  (let [v (/ (:elapsed val) 1e9)
        [scale unit] (criterium/scale-time v)]
    (format "%32s: %s\n" "Elapsed time" (criterium/format-value v scale unit))))

(defn- format-count-time [[k {c :count t :time}]]
  (let [[scale unit] (criterium/scale-time t)]
    (format "%36s:  count %d  time %2.1f %s\n" (name k) c (/ t scale) unit)))

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
    (for [[mk v] vs
          :let [[scale unit] (criterium/scale-memory v)]]
      (format "%40s: %2.1f %s\n" (name mk) (double (/ v scale)) unit))))

(defmethod format-metric :memory
  [_ val]
  (format "%32s:\n%s" "Memory"
          (apply str (map format-memory-metrics val))))

(defn- format-runtime-memory-metrics [[k v]]
  (let [[scale unit] (criterium/scale-memory v)]
    (format "%36s: %2.1f %s\n" (name k) (double (/ v scale)) unit)))

(defmethod format-metric :runtime-memory
  [_ val]
  (format "%32s:\n%s" "Runtime Memory"
          (apply str (map format-runtime-memory-metrics val))))

(defmethod format-metric :compilation
  [_ val]
  (let [v (/ (:compilation-time val) 1e9)
        [scale unit] (criterium/scale-time v)
        ]
    (format "%32s: %2.1f %s\n" "JIT Compilation time" (* v scale) unit)))

(defn format-count [[k v]]
  (format "%36s: %d\n" (name k) v))

(defmethod format-metric :class-loader
  [_ val]
  (apply
    str
    (format "%32s:\n" "Classloader")
    (map format-count val)))

(defn- wrap-for-metric [expr stat]
  `(~(metrics stat) ~expr))

(defn print-metrics [metrics]
  (doseq [[k v] metrics]
    (print (format-metric k v))))

(defmacro time
  "Evaluates expr and prints the time it took.
  Return the value of expr.

  The timing info is available as a data structure by calling last-time."
  {:added "1.0"}
  [expr & options]
  (let [options   (apply hash-map options)
        use-metrics (let [use-metrics (:metrics options [:time])]
                    (if (= :all use-metrics) (keys metrics) use-metrics))]
    `(let [vals#   (toolkit/instrumented
                   ~(reduce wrap-for-metric
                            `(toolkit/with-expr-value ~expr)
                            use-metrics))
           deltas# (toolkit/deltas (dissoc vals# :expr-value))]
       (vreset! last-time* deltas#)
       (print-metrics deltas#)
       (:expr-value vals#))))


(defn last-time
  "Return the data from the last time invocation."
  []
  @last-time*)
