(ns criterium.core-test
  (:require [clojure.test :refer [deftest is]]
            [criterium
             [core :as core]]))

(deftest outliers-test
  (is (= (core/outlier-count 0 0 0 0)
         (core/outliers [1 2 5 7 8])))
  (is (= (core/outlier-count 0 0 0 0)
         (core/outliers [1 2 2 5 7 8])))
  (is (= (core/outlier-count 1 0 0 0)
         (core/outliers [-100 1 2 5 7 8 9])))
  (is (= (core/outlier-count 0 1 0 0)
         (core/outliers [-10 1 2 5 7 8 9])))
  (is (= (core/outlier-count 0 0 1 0)
         (core/outliers [1 1 2 5 7 8 22])))
  (is (= (core/outlier-count 0 0 0 1)
         (core/outliers [1 1 2 5 7 8 100]))))

(deftest outlier-effect-test
  (is (= :unaffected (core/outlier-effect 0.009)))
  (is (= :slight (core/outlier-effect 0.09)))
  (is (= :moderate (core/outlier-effect 0.49)))
  (is (= :severe (core/outlier-effect 0.51))))

(deftest outlier-significance-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (is (= 0.9960022873987793
         (core/outlier-significance
          [1.395522860870968 []]
          [(* 0.0013859776344426547 0.0013859776344426547) []]
          67108864))))

(deftest bench-test
  (let [s (with-out-str
            (core/bench 1 :target-execution-time 1))]
    (is s)))
