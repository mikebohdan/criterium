(ns criterium-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [criterium :as criterium]
            [criterium
             [config :as config]
             [pipeline :as pipeline]]))

(deftest config-map-test
  (is (= config/default-config (criterium/config-map [])))
  (is (= (assoc config/default-config
                :pipeline {:stages [:class-loader] :terminator :elapsed-time-ns})
         (criterium/config-map [:pipeline [:class-loader]])))
  (is (= (merge config/default-config {:processing [:samples] :sample-mode :one-shot})
         (criterium/config-map [:sample-mode :one-shot]))))

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
