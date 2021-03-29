(ns criterium.time-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [criterium.config :as config]
   [criterium.jvm :as jvm]
   [criterium.pipeline :as pipeline]
   [criterium.time :as criterium]))

(deftest config-map-test
  (is (= (-> config/default-config
             (#'config/add-metrics)
             (#'config/ensure-pipeline-stages))
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
             (assoc-in [:pipeline :stages] [])
             (assoc
              :sample-scheme (config/one-shot-sample-scheme {})
              :analysis []
              :report [{:report-type :metrics}])
             (#'config/add-metrics))
         (criterium/config-map
          {:sample-scheme :one-shot}))))

(deftest time-test
  (testing "time"
    (vreset! criterium/last-time* nil)
    (is (nil? (criterium/last-time)))
    (let [out (with-out-str (criterium/time 1))]
      (testing "outputs the estimated time on stdout"
        (is (re-find #"\s+Elapsed Time: [0-9.]+ ± [0-9.]+ [mn]s" out)))
      (testing "makes the timing data available with last-time"
        (is (s/conform ::pipeline/sample (criterium/last-time))))))
  (testing "time with stats"
    (let [out (with-out-str (criterium/time 1 :limit-eval-count 10000))]
      (testing "outputs statistics on stdout"
        (is (re-find #"±" out)))))
  (testing "time with one-shot"
    (let [out (with-out-str (criterium/time 1 :sample-scheme :one-shot))]
      (testing "outputs statistics on stdout"
        (is (not (re-find #"±" out))))))
  (testing "time with :time-fn option"
    (let [out (with-out-str (criterium/time
                             1
                             :time-fn jvm/current-thread-cpu-time))]
      (testing "outputs statistics on stdout"
        (is (re-find #"±" out))))))
