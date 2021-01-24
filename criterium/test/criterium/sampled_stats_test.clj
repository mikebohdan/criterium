(ns criterium.sampled-stats-test
  (:require
   [clojure.test :refer [deftest is]]
   [criterium.sampled-stats :as sampled-stats]))

(deftest stats-for-test
  (let [samples (mapv #(hash-map :v %) (repeat 100 1))
        stats (sampled-stats/stats-for [:v] 1 samples {})]
    (is (= [1.0 [1.0 1.0]] (-> stats :mean)))
    (is (= [0.0 [0.0 0.0]] (-> stats :variance)))))
