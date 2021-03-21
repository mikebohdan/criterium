(ns criterium.platform
  "Platform characterisation"
  (:require
   [clojure.pprint :as pp]
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

;;; nanoTime granularity

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

;;; Minimum measured time

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

;;; platform description

(defn- mean-elapsed-time-ns [result]
  (-> result :stats :elapsed-time-ns :mean))

(defn- min-elapsed-time-ns [result]
  (-> result :stats :elapsed-time-ns :min))

(defn platform-stats
  "Return a sequence of estimates for times that describe accuracy of timing.

  Each estimate has a :name value."
  ([] (platform-stats {}))
  ([options]
   (let [options (merge
                  {:report       []
                   :limit-time-s 10}
                  options
                  {:return-value :stats})
         ]
     [(assoc (nanotime-latency options) :name "latency")
      (assoc (nanotime-granularity options) :name "granularity")
      (assoc (constant-long options) :name "constant-long")
      (assoc (constant-double options) :name "constant-double")
      (assoc (constant-object options) :name "constant-object")
      (assoc (constant-nil options) :name "constant-nil")])))

(defn platform-point-estimates
  "Return estimates for times that describe the accuracy of timing.

  The latency and granularity are min estimate, and the the rest are
  mean estimates."
  ([] (platform-point-estimates {}))
  ([options]
   (let [stats          (platform-stats options)
         point-estimate {:latency         min-elapsed-time-ns
                         :granularity     min-elapsed-time-ns
                         :constant-long   mean-elapsed-time-ns
                         :constant-double mean-elapsed-time-ns
                         :constant-object mean-elapsed-time-ns
                         :constant-nil    mean-elapsed-time-ns}]
     (reduce
      (fn [res stat]
        (let [kw (keyword (:name stat))]
          (assoc res kw ((point-estimate kw) stat))))
      {}
      stats))))

(defn -main
  "Output a table of the platform min and mean point estimates."
  []
  (let [stats
        (reduce
         (fn [res stat]
           (let [view {:name    (:name stat)
                       :min-ns  (min-elapsed-time-ns stat)
                       :mean-ns (mean-elapsed-time-ns stat)}]
             (conj res view)))
         []
         (platform-stats))]
    (pp/print-table stats)))
