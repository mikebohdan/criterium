(ns criterium.config-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [criterium.config :as config]))

(deftest default-options-test
  (testing "default-options"
    (is (or
         (s/valid? ::config/options config/default-options)
         (s/explain ::config/options config/default-options)))))

(deftest expand-options-test
  (testing "expand-options"
    (testing "sets processing to :stats for :sample-mode :full"
      (let [options (config/expand-options {:sample-mode :full})]
        (is (s/valid? ::config/options options))
        (is (= [:stats] (:analysis options)))))
    (testing "sets processing to :sample for :sample-mode :one-shot"
      (let [options (config/expand-options {:sample-scheme {:sample-mode :one-shot}})]
        (is (s/valid? ::config/options options))
        (is (= [:samples] (:analysis options)))))))
