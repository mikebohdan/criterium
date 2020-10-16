(ns criterium.pipeline-test
  (:require [clojure.set :as set]
            [clojure.spec.test.alpha]
            [criterium
             [measured :as measured]
             [pipeline :as pipeline]]
            [clojure.test :refer [deftest is testing]]))

(clojure.spec.test.alpha/check
 [`pipeline/time-metric
  `pipeline/with-class-loader-counts
  `pipeline/with-compilation-time
  `pipeline/with-memory
  `pipeline/with-runtime-memory
  `pipeline/with-finalization-count
  `pipeline/with-garbage-collector-stats
  `pipeline/elapsed-time
  `pipeline/total-memory
  `pipeline/divide
  `pipeline/pipeline
  `pipeline/execute])

(def m-value 12345)
(def m (measured/measured
        (fn [] ::state)
        (fn [state _n]
          [m-value [state state]])
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
        (is (= m-value (:elapsed-time-ns res))))
      (testing "Has the evaluation count on the :eval-count key"
        (is (= m-value (:elapsed-time-ns res))))
      (testing "Has the measured expression value on the :expr-value key"
        (is (= m-value (:elapsed-time-ns res)))))))

(deftest pipeline-fns-test
  (doseq [[kw f] pipeline/pipeline-fns]
    (testing (str "Pipeline function " (name kw))
      (let [res (pipeline/execute
                 (f pipeline/time-metric)
                 m
                 1)
            ks (set (keys res))]
        (is (= base-keys (set/intersection base-keys ks)))))))

(deftest pipeline-test
  (testing "pipeline"
    (testing "builds a pipeline"
      (is (fn? (pipeline/pipeline
                {:stages (keys pipeline/pipeline-fns)
                 :terminator (first (keys pipeline/terminal-fns))}))))
    (testing "throws if passed a non keyword"
      (is (thrown? clojure.lang.ExceptionInfo
                   (pipeline/pipeline
                    {:stages [::unknown]
                     :terminator (first (keys pipeline/terminal-fns))}))))
    (testing "throws if passed an unknown terminal function"
      (is (thrown? clojure.lang.ExceptionInfo
                   (pipeline/pipeline
                    {:stages (keys pipeline/pipeline-fns)
                     :terminator ::unknown}))))))
