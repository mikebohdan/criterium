(ns criterium.platform
  "Platform characterisation"
  (:require
   [criterium :as criterium]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

;;; nanoTime latency

(defn nanotime-latency
  ([] (nanotime-latency {}))
  ([options]
   (criterium/measure
    (measured/expr (jvm/timestamp))
    (criterium/config-map
     (merge
      {:limit-time-s 10}
      options)))))

(defn- nanotime-granularity-fn
  [_ ^long eval-count]
  (let [start (jvm/timestamp)]
    (loop [n eval-count
           t (jvm/timestamp)]
      (let [t1 (jvm/timestamp)]
        (if (= t t1)
          (recur n t1)
          (if (pos? n)
            (recur (unchecked-dec n) t1)
            t1))))
    (let [finish (jvm/timestamp)]
      [(unchecked-subtract finish start) nil])))

(defn nanotime-granularity-measured
  []
  (let [state-fn (fn granularity-state-fn [] nil)]
    (measured/measured
     state-fn
     nanotime-granularity-fn)))

(defn nanotime-granularity
  ([] (nanotime-granularity {}))
  ([options]
   (criterium/measure
    (nanotime-granularity-measured)
    (criterium/config-map
     (merge
      {:limit-time-s 10}
      options)))))

;; Minimum measured time
(defn constant-bench
  ([] (constant-bench {}))
  ([options]
   (criterium/measure
    (measured/expr 1)
    (criterium/config-map
     (merge
      {:limit-time-s 10}
      options)))))

(defn empty-bench
  ([] (empty-bench {}))
  ([options]
   (criterium/measure
    (measured/expr nil)
    (criterium/config-map
     (merge
      {:limit-time-s 10}
      options)))))

(defn- mean-elapsed-time-ns [result]
  (-> result :stats :stats :elapsed-time-ns :mean first))

(defn platform-stats
  "Return mean estimates for times that describe the accuracy of timing."
  []
  (let [latency     (nanotime-latency)
        granularity (nanotime-granularity)
        constant    (constant-bench)
        empty       (empty-bench)]
    {:latency     (mean-elapsed-time-ns latency)
     :granularity (mean-elapsed-time-ns granularity)
     :constant    (mean-elapsed-time-ns constant)
     :empty       (mean-elapsed-time-ns empty)}))

;; (platform-stats)
