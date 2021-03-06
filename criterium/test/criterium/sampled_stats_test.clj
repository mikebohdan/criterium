(ns criterium.sampled-stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.sampled-stats :as sampled-stats]))

(deftest stats-for-test
  (let [samples (mapv #(hash-map :v %) (repeat 100 1))
        stats   (sampled-stats/stats-for [:v] 1 samples {})]
    (is (= 1.0 (-> stats :mean)))
    (is (= 0.0 (-> stats :variance))))

  (testing "stats on [0..100]"
    (let [samples (mapv #(hash-map :v %) (range 101))
          stats   (sampled-stats/stats-for
                   [:v] 1 samples {:tail-quantile 0.05})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance)))))

  (testing "stats on (reverse [0..100])"
    (let [samples (mapv #(hash-map :v %) (reverse (range 101)))
          stats   (sampled-stats/stats-for
                   [:v] 1 samples {:tail-quantile 0.05})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance)))))

  (testing "stats on [0..100]*2"
    (let [samples (mapv #(hash-map :v %) (range 0 202 2))
          stats   (sampled-stats/stats-for
                   [:v] 2 samples {:tail-quantile 0.05})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance))))))

(deftest bootstrap-stats-for-test
  (testing "constant input"
    (let [samples (mapv #(hash-map :v %) (repeat 100 1))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {:tail-quantile 0.05})]
      (is (= [1.0 [1.0 1.0]] (-> stats :mean)))
      (is (= [1.0 [1.0 1.0]] (-> stats :0.25)))
      (is (= [1.0 [1.0 1.0]] (-> stats :0.75)))
      (is (= [1.0 [1.0 1.0]] (-> stats :lower-tail)))
      (is (= [1.0 [1.0 1.0]] (-> stats :upper-tail)))
      (is (= [0.0 [0.0 0.0]] (-> stats :variance)))))

  (testing "sequential input"
    (let [samples (mapv #(hash-map :v %) (range 101))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {})]
      (let [[m [l u]] (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [[m [l u]] (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u)))))

  (testing "reverse sequential input"
    (let [samples (mapv #(hash-map :v %) (reverse (range 101)))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {})]
      (let [[m [l u]] (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [[m [l u]] (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u))))))
