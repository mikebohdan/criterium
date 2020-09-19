(ns criterium.chart-test
  (:require [criterium.chart :as chart]
            [clojure.test :refer [deftest is testing]]))

(deftest bin-defintiion-test
  (is (= {:n-bins 5 :delta 2.0 :mn 0.0 :mx 10.0}
         (chart/bin-definition 0 10 10 {})))
  (is (= {:n-bins 10 :delta 1.0 :mn 0.0 :mx 10.0}
         (chart/bin-definition 0 10 10 {:num-bins 10})))
  (is (= {:n-bins 10 :delta 1.0 :mn 0.0 :mx 10.0}
         (chart/bin-definition 0 10 10 {:delta 1})))
  (is (= {:n-bins 10 :delta 2.0 :mn 0.0 :mx 10.0}
         (chart/bin-definition 0 10 10 {:delta 2 :num-bins 10}))))
