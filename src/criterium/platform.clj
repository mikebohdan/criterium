(ns criterium.platform
  "Platform characterisation"
  (:require [criterium
             [eval :as eval]
             [jvm :as jvm]
             [time :as time]
             [toolkit :as toolkit]]))


;;; nanoTime latency

(defn nanotime-latency-fn
  ^long [^long x]
  (loop [i (dec x)]
    (jvm/timestamp)
    (when (pos? i)
      (recur (unchecked-dec i))))
  (jvm/timestamp))


(defn nanotime-latency-measured
  [^long n]
  (toolkit/measured
    (fn ^long [] n)
    nanotime-latency-fn
    n))


(defn nanotime-latency [n & [options]]
  (time/measure*
    (nanotime-latency-measured (long n))
    (merge
      {:limit-time 1}
      options)))


;;; nanoTime granularity

(defn- nanotime-granularity-fn
  ^long [^long eval-count]
  (loop [n eval-count
         t (jvm/timestamp)]
    (let [t1 (jvm/timestamp)]
      (if (= t t1)
        (recur n t1)
        (if (pos? n)
          (recur (unchecked-dec n) t1)
          t1)))))


(defn nanotime-granularity-measured
  [eval-count]
  (let [state-fn (fn granularity-state-fn [] eval-count)]
    (toolkit/measured
      state-fn
      nanotime-granularity-fn
      eval-count)))


(defn nanotime-granularity [n & [options]]
  (time/measure*
    (nanotime-granularity-measured (long n))
    (merge
      {:limit-time 1}
      options)))


;; Minimum measured time
(defn constant-bench [& [options]]
  (time/measure*
    (toolkit/measured
      (fn ^long [] 1)
      (fn ^long [^long x] x)
      1)
    (merge
      {:limit-time 1}
      options)))

(defn empty-bench [& [options]]
  (time/measure*
    (toolkit/measured
      (fn ^long [] 0)
      (fn [^long x])
      1)
    (merge
      {:limit-time 1}
      options)))

(defn platform-stats
  "Return mean estimates for times that describe the accuracy of timing."
  []
  (let [latency (nanotime-latency 1000)
        granularity (nanotime-granularity 100)
        constant (constant-bench)
        empty (empty-bench)]
    {:latency (-> latency :stats :mean first)
     :granularity (-> granularity :stats :mean first)
     :constant (-> constant :stats :mean first)
     :empty (-> empty :stats :mean first)}))

;; (platform-stats)
