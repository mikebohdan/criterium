(ns criterium.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.analyze :as analyze]
   [criterium.core :as core]))

(deftest outliers-test
  (is (= (analyze/outlier-count 0 0 0 0)
         (core/outliers [1 2 5 7 8])))
  (is (= (analyze/outlier-count 0 0 0 0)
         (core/outliers [1 2 2 5 7 8])))
  (is (= (analyze/outlier-count 1 0 0 0)
         (core/outliers [-100 1 2 5 7 8 9])))
  (is (= (analyze/outlier-count 0 1 0 0)
         (core/outliers [-10 1 2 5 7 8 9])))
  (is (= (analyze/outlier-count 0 0 1 0)
         (core/outliers [1 1 2 5 7 8 22])))
  (is (= (analyze/outlier-count 0 0 0 1)
         (core/outliers [1 1 2 5 7 8 100]))))

(deftest bench-test
  (let [s (with-out-str
            (core/bench 1 :target-execution-time 1))]
    (is s)))
