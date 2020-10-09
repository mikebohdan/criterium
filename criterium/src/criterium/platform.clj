(ns criterium.platform
  "Platform characterisation"
  (:require [criterium
             [jvm :as jvm]
             [measure :as measure]
             [measured :as measured]]))


;;; nanoTime latency

;; (defn nanotime-latency-fn
;;   ^long [^long x]
;;   (loop [i (dec x)]
;;     (jvm/timestamp)
;;     (when (pos? i)
;;       (recur (unchecked-dec i))))
;;   (jvm/timestamp))


;; (defn nanotime-latency-measured
;;   [^long n]
;;   (measured/measured
;;     (fn ^long [] n)
;;     nanotime-latency-fn
;;     n))


(defn nanotime-latency [& [options]]
  (measure/measure
    ;;(nanotime-latency-measured (long n))
   (measured/expr (jvm/timestamp))
   (merge
    {:limit-time 10}
    options)))

;; (nanotime-latency)

;;; nanoTime granularity

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

(defn nanotime-granularity [& [options]]
  (measure/measure
   (nanotime-granularity-measured)
   (merge
    {:limit-time 10}
    options)))
;; (nanotime-granularity)


;; Minimum measured time
(defn constant-bench [& [options]]
  (measure/measure
   (measured/expr 1)
    ;; (measured/measured
    ;;   (fn [] 1)
    ;;   (fn [x] x)
    ;;   1)
   (merge
    {:limit-time 10
       ;; :sink-fn criterium.eval/sink-primitive-long
     }
    options)))
;; (constant-bench)


(def m (measured/expr 1))

;; (def mb (measured/batch
;;          m
;;          100))

#_(comment
    (require 'no.disassemble)
    (pprint/pprint
     (println (no.disassemble/disassemble-str (:f m)))
     (println (no.disassemble/disassemble-str (:f mb)))
     (println (no.disassemble/disassemble-str nanotime-latency-fn))
     (println (no.disassemble/disassemble-str (criterium.eval.ObjectSink. 1 1 nil)))
     (println (no.disassemble/disassemble-str (toolkit/with-time)))))

(defn empty-bench [& [options]]
  (measure/measure
   (measured/expr nil)
   (merge
    {:limit-time 10}
    options)))

;; (empty-bench)

(defn platform-stats
  "Return mean estimates for times that describe the accuracy of timing."
  []
  (let [latency (nanotime-latency)
        granularity (nanotime-granularity)
        constant (constant-bench)
        empty (empty-bench)]
    {:latency (-> latency :stats :time :mean first)
     :granularity (-> granularity :stats :time :mean first)
     :constant (-> constant :stats :time :mean first)
     :empty (-> empty :stats :time :mean first)}))

;; (platform-stats)
