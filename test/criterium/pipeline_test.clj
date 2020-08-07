(ns criterium.pipeline-test
  (:require [criterium
             [measured :as measured]
             [pipeline :as pipeline]]
            [clojure.test :refer [deftest is]]))


(def m (measured/measured
         (fn [] ::state)
         (fn [state _n]
           [::time [state state]])
         nil))

(deftest execute-test
  (is (= {:state           ::state
          :expr-value      [::state ::state]
          :eval-count      1
          :elapsed-time-ns :criterium.pipeline-test/time}
         (pipeline/execute
           pipeline/time-metric
           m
           1))))
