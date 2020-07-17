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


(defn nanotime-latency [^long n]
  (time/measure*
    (nanotime-latency-measured n)
    {:limit-time 1}))


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


(defn nanotime-granularity [^long n]
  (time/measure*
    (nanotime-granularity-measured n)
    {:limit-time 1}))


;; Minimum measured time
(defn constant-bench []
  (time/measure*
    (toolkit/measured
      (fn ^long [] 1)
      (fn [^long x] x)
      1)
    {:limit-time 1}))


(defn platform-stats
  []
  (let [latency (nanotime-latency 1000)
        granularity (nanotime-granularity 100)
        constant (constant-bench)]
    {:latency (-> latency :stats :mean first)
     :granularity (-> granularity :stats :mean first)
     :constant (-> constant :stats :mean first)}))

;; (platform-stats)
