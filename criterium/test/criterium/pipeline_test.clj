(ns criterium.pipeline-test
  (:require [clojure.set :as set]
            [criterium
             [measured :as measured]
             [pipeline :as pipeline]]
            [clojure.test :refer [deftest is testing]]))

(def m (measured/measured
        (fn [] ::state)
        (fn [state _n]
          [::time [state state]])
        nil))

(def base-keys #{:state :expr-value :eval-count :elapsed-time-ns})

(deftest execute-test
  (testing "Execute a measured with time-metric"
    (let [res (pipeline/execute
               pipeline/time-metric
               m
               1)]
      (testing "Has the measured state on the :state key"
        (is (= ::state (:state res))))
      (testing "Has the measured time on the :elapsed-time-ns key"
        (is (= ::time (:elapsed-time-ns res))))
      (testing "Has the evaluation count on the :eval-count key"
        (is (= ::time (:elapsed-time-ns res))))
      (testing "Has the measured expression value on the :expr-value key"
        (is (= ::time (:elapsed-time-ns res)))))))

(deftest pipeline-fns-test
  (doseq [[kw f] pipeline/pipeline-fns]
    (testing (str "Pipeline function " (name kw))
      (let [res (pipeline/execute
                 (f pipeline/time-metric)
                 m
                 1)
            ks (set (keys res))]
        (is (= base-keys (set/intersection base-keys ks)))))))
