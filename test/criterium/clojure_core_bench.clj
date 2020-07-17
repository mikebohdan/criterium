(ns criterium.clojure-core-bench
  (:require  [clojure.test :as t]
             [clojure.test.check
              [generators :as gen]]
             [criterium
              [arg-gen :as arg-gen]
              [time :as time]]))


(defn nth-bench [mx]
  (arg-gen/for-all [v (gen/vector gen/int 1 mx)
                    i (gen/choose 0 (dec (count v)))]
    (nth v i)))

(defn vec-nth-bench [mx]
  (arg-gen/for-all [v (gen/vector gen/int 1 mx)
                    i (gen/choose 0 (dec (count v)))]
    (.nth ^clojure.lang.APersistentVector v ^int i)))

(criterium.time/measure* (nth-bench 10000) {:limit-time 1})
(criterium.time/measure* (vec-nth-bench 10000) {:limit-time 1})

(criterium.chart/view
  (criterium.chart/time-histogram
    (criterium.time/measure*
      (vec-nth-bench 10000)
      {:limit-time 1
       :return-samples true})))
