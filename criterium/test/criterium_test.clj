(ns criterium-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [criterium :as criterium]
            [criterium
             [measure :as measure]
             [pipeline :as pipeline]]))

(deftest options-map-test
  (is (= measure/default-options (criterium/options-map [])))
  (is (= (assoc measure/default-options
                :pipeline {:stages [:class-loader] :terminator :elapsed-time-ns})
         (criterium/options-map [:pipeline [:class-loader]])))
  (is (= (merge measure/default-options {:processing [:samples] :sample-mode :one-shot})
         (criterium/options-map [:sample-mode :one-shot]))))

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
