(ns criterium.measure-test
  (:require [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest is testing]]
            [criterium.measure :as measure]))

(deftest default-options-test
  (testing "default-options"
    (is (or
         (s/valid? ::measure/options measure/default-options)
         (s/explain ::measure/options measure/default-options)))))

(deftest expand-options-test
  (testing "expand-options"
    (testing "sets processing to :stats for :sample-mode :full"
      (let [options (measure/expand-options {:sample-mode :full})]
        (is (s/valid? ::measure/options options))
        (is (= [:stats] (:analysis options)))))
    (testing "sets processing to :sample for :sample-mode :one-shot"
      (let [options (measure/expand-options {:sample-scheme {:sample-mode :one-shot}})]
        (is (s/valid? ::measure/options options))
        (is (= [:samples] (:analysis options)))))))
