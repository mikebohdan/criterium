(ns criterium.analyze-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyze :as analyze]))


(deftest outlier-significance-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier significance"
    (let [batch-size 67108864]
      (is (= 0.9960022873987793
             (analyze/outlier-significance
              (/ 1.395522860870968 batch-size)
              (/ (* 0.0013859776344426547 0.0013859776344426547)
                 batch-size)
              batch-size))))))
