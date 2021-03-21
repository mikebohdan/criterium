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
   (criterium/time-measured
    (measured/expr (jvm/timestamp))
    (merge
     {:sample-scheme {:scheme-type     :full
                      :limit-time-s    10
                      :thread-priority :max-priority}
      :return-value  ::nil}
     options))))

(defn- nanotime-granularity-fn
  [_ ^long eval-count]
  (let [start  (jvm/timestamp)
        finish (loop [n eval-count
                      t start]
                 (let [t1 (jvm/timestamp)]
                   (if (= t t1)
                     (recur n t1)
                     (if (pos? n)
                       (recur (unchecked-dec n) t1)
                       t1))))
        delta  (unchecked-subtract finish start)]
    [delta (long (/ delta eval-count))]))

(defn nanotime-granularity-measured
  []
  (let [state-fn (fn granularity-state-fn [] nil)]
    (measured/measured
     state-fn
     nanotime-granularity-fn)))

(defn nanotime-granularity
  ([] (nanotime-granularity {}))
  ([options]
   (criterium/time-measured
    (nanotime-granularity-measured)
    (merge
     {:sample-scheme {:scheme-type     :full
                      :limit-time-s    10
                      :thread-priority :max-priority}
      :return-value  ::nil}
     options))))

;; Minimum measured time
(defn constant-long
  ([] (constant-long {}))
  ([options]
   (criterium/time-measured
    (measured/expr 1)
    (merge
     {:limit-time-s 10}
     options))))

(defn constant-double
  ([] (constant-double {}))
  ([options]
   (criterium/time-measured
    (measured/expr 1.0)
    (merge
     {:limit-time-s 10}
     options))))

(defn constant-object
  ([] (constant-double {}))
  ([options]
   (criterium/time-measured
    (measured/expr {})
    (merge
     {:limit-time-s 10}
     options))))

(defn constant-nil
  ([] (constant-nil {}))
  ([options]
   (criterium/time-measured
    (measured/expr nil)
    (merge
     {:limit-time-s 10}
     options))))

(defn- mean-elapsed-time-ns [result]
  (-> result :stats :elapsed-time-ns :mean first))

(defn- min-elapsed-time-ns [result]
  (-> result :stats :elapsed-time-ns :min first))

(defn platform-stats
  "Return mean estimates for times that describe the accuracy of timing."
  ([] (platform-stats {}))
  ([options]
   (let [options         (merge
                          {:report       []
                           :limit-time-s 10}
                          options
                          {:return-value :stats})
         latency         (nanotime-latency options)
         granularity     (nanotime-granularity options)
         constant-long   (constant-long options)
         constant-double (constant-double options)
         constant-object (constant-object options)
         constant-nil    (constant-nil options)]
     {:latency         (min-elapsed-time-ns latency)
      :granularity     (min-elapsed-time-ns granularity)
      :constant-long   (mean-elapsed-time-ns constant-long)
      :constant-double (mean-elapsed-time-ns constant-double)
      :constant-object (mean-elapsed-time-ns constant-object)
      :constant-nil    (mean-elapsed-time-ns constant-nil)})))

;; (platform-stats)
