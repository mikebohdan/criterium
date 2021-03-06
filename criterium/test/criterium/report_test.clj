(ns criterium.report-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.report :as report]))

(deftest print-stat-test
  (testing "print-stat"
    (is (= "Elapsed Time: 100 ± 12.0 ns"
           (str/trim
            (with-out-str
              (report/print-stat
               [:elapsed-time-ns]
               {:mean     100.0
                :variance 16.0})))))))

(deftest print-booststrap-stat-test
  (testing "print-bootstrap-stat"
    (is (= ["Elapsed Time: 100 ns CI [95.0 105] (0.050 0.950)"
            "Elapsed Time σ: 4.00 ns CI [3.00 5.00] (0.050 0.950)"]
           (mapv
            str/trim
            (str/split-lines
             (with-out-str
               (report/print-bootstrap-stat
                [:elapsed-time-ns]
                {:mean     {:point-estimate 100.0
                            :estimate-quantiles
                            [{:value 95.0 :alpha 0.05}
                             {:value 105.0 :alpha 0.95}]}
                 :variance {:point-estimate 16.0
                            :estimate-quantiles
                            [{:value 9.0 :alpha 0.05}
                             {:value 25.0 :alpha 0.95}]}}))))))))
