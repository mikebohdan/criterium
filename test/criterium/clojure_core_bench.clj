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
    (.nth ^clojure.lang.IPersistentVector v ^int i)))

(criterium.time/measure*
  (nth-bench 10000)
  {:limit-time 10})

(criterium.time/measure*
  (vec-nth-bench 10000)
  {:limit-time 10})


(defn vector-destructure-bench [mx]
  (arg-gen/for-all [v (gen/vector gen/int 3 mx)
                    i (gen/choose 0 (dec (count v)))]
    (let [[a b c] v]
      (+ a b c))))

(defn vector-explicit-destructure-bench [mx]
  (arg-gen/for-all [v (gen/vector gen/int 3 mx)
                    i (gen/choose 0 (dec (count v)))]
    (+ (.nth ^clojure.lang.IPersistentVector v 0)
       (.nth ^clojure.lang.IPersistentVector v 1)
       (.nth ^clojure.lang.IPersistentVector v 2))))

(let [^clojure.lang.IPersistentVector v [1 2 3]
      state-fn (fn ^clojure.lang.IPersistentVector [] v)
      f (fn [^clojure.lang.IPersistentVector v]
          (.nth  v  2))
      measured (criterium.toolkit/measured state-fn f 1)]
  (criterium.time/measure*
    measured
    {:limit-time 10}))

(let [^clojure.lang.IPersistentVector v [1 2 3]
      state-fn (fn ^clojure.lang.IPersistentVector [] v)
      f (fn [^clojure.lang.IPersistentVector v]
          (nth v 2))
      measured (criterium.toolkit/measured state-fn f 1)]
  (criterium.time/measure*
    measured
    {:limit-time 10}))

(criterium.time/measure*
  (criterium.toolkit/measured-batch
    (vector-destructure-bench 3)
    1000)
  {:limit-time 10})

(criterium.time/measure*
  (vector-explicit-destructure-bench 3)
  {:limit-time 10})


(criterium.chart/view
  (criterium.chart/time-histogram
    (criterium.time/measure*
      (vec-nth-bench 10000)
      {:limit-time 1
       :return-samples true})))

(require '[criterium.core :refer [report-result benchmark]])

(comment
  (let [v [1 2 3]]
    (report-result
      (benchmark
        (.nth ^IPersistentVector v 2) {})))) ;; 4.049624 ns

(comment
  (let [v [1 2 3]]
    (report-result
      (benchmark
        (nth v 2) {})))) ;; 7.456217 ns
