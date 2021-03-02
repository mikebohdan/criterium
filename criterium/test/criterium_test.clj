(ns criterium-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [criterium :as criterium]
   [criterium.config :as config]
   [criterium.jvm :as jvm]
   [criterium.pipeline :as pipeline]
   [criterium.sample-scheme :as sample-scheme]))

(deftest config-map-test
  (is (= (-> (assoc-in config/default-config
                       [:pipeline :stages]
                       (sample-scheme/required-stages {:scheme-type :full}))
             (#'config/add-metrics))
         (criterium/config-map {})))
  (is (= (-> (assoc config/default-config
                    :pipeline {:stages     [:class-loader
                                            :compilation-time
                                            :garbage-collector]
                               :terminator :elapsed-time-ns})
             (#'config/add-metrics))
         (criterium/config-map
          {:pipeline [:class-loader
                      :compilation-time
                      :garbage-collector]})))
  (is (= (-> config/default-config
             (assoc-in [:pipeline :stages]
                       [:compilation-time :garbage-collector])
             (#'config/add-metrics))
         (criterium/config-map
          {:sample-mode :one-shot}))))

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
        (is (re-find #"GC count" out)))))
  (testing "time with :time-fn option"
    (let [out (with-out-str (criterium/time
                             1
                             :time-fn jvm/current-thread-cpu-time))]
      (testing "outputs statistics on stdout"
        (is (re-find #"GC count" out))))))
