(ns criterium-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [criterium :as criterium]
            [criterium
             [pipeline :as pipeline]]))

(deftest time-test
  (testing "time"
    (vreset! criterium/last-time* nil)
    (is (nil? (criterium/last-time)))
    (let [out (with-out-str (criterium/time 1))]
      (testing "outputs the estimated time on stdout"
        (is (re-find #"\s+Elapsed Time: [0-9.]+ ± [0-9.]+ [mn]s" out)))
      (testing "makes the timing data available with last-time"
        (is (s/conform ::pipeline/sample (criterium/last-time))))))
  (testing "time with :stats option"
    (let [out (with-out-str (criterium/time 1 :times 10000))]
      (testing "outputs statistics on stdout"
        (is (re-find #"xyz" out))))))
