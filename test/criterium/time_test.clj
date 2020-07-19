(ns criterium.time-test
  (:require [criterium
             [time :as time]]
            [clojure.test :refer [deftest is testing]]))



(deftest stats-for-test
  (let [samples (mapv #(hash-map :v %) (repeat 100 1))
        stats (time/stats-for [:v] 1 samples {})]
    (is (= [1.0 [1.0 1.0]] (-> stats :mean)))
    (is (= [0.0 [0.0 0.0]] (-> stats :variance)))))
