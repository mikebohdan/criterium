(ns criterium.config-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [criterium.config :as config]))

(deftest default-options-test
  (testing "default-options"
    (is (or
         (s/valid? ::config/config config/default-config)
         (s/explain ::config/config config/default-config)))))

(deftest expand-options-test
  (testing "expand-options"
    (testing "sets processing to :stats for :sample-mode :full"
      (let [config (config/expand-options
                    {:sample-scheme {:scheme-type     :full
                                     :max-gc-attempts 3}})]
        (is (s/valid? ::config/config config)
            (s/explain-str (s/get-spec ::config/config) config))
        (is (= [{:analysis-type  :stats
                 :tail-quantile  0.025
                 :bootstrap-size 100}]
               (:analysis config)))))
    (testing "sets processing to :sample for :sample-mode :one-shot"
      (let [config (config/expand-options
                    {:sample-scheme {:scheme-type     :one-shot
                                     :max-gc-attempts 3}})]
        (is (s/valid? ::config/config config)
            (s/explain-str (s/get-spec ::config/config) config))
        (is (= [{:analysis-type :samples}]
               (:analysis config)))))))
