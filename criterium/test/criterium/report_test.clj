(ns criterium.report-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyze :as analyze]
   [criterium.report :as report]))

(deftest print-stat-test
  (testing "print-stat"
    (is (= "Elapsed Time: 100 ns  3σ [88.0 112]  min 89.0"
           (str/trim
            (with-out-str
              (report/print-stat
               [:elapsed-time-ns]
               {:mean              100.0
                :variance          16.0
                :mean-plus-3sigma  112.0
                :mean-minus-3sigma 88.0
                :min               89.0})))))))

(defn- trimmed-lines
  [s]
  (->> s
       str/split-lines
       (mapv str/trim)))

(deftest print-booststrap-stat-test
  (testing "print-bootstrap-stat"
    (is (= ["Elapsed Time min: 16.0 ns CI [9.00 25.0] (0.050 0.950)"
            "Elapsed Time mean: 100 ns CI [95.0 105] (0.050 0.950)"
            "Elapsed Time 3σ: [76.0 124] ns"]
           (trimmed-lines
            (with-out-str
              (report/print-bootstrap-stat
               [:elapsed-time-ns]
               {:mean              {:point-estimate 100.0
                                    :estimate-quantiles
                                    [{:value 95.0 :alpha 0.05}
                                     {:value 105.0 :alpha 0.95}]}
                :variance          {:point-estimate 16.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}
                :min               {:point-estimate 16.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}
                :mean-plus-3sigma  {:point-estimate 124.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}
                :mean-minus-3sigma {:point-estimate 76.0
                                    :estimate-quantiles
                                    [{:value 9.0 :alpha 0.05}
                                     {:value 25.0 :alpha 0.95}]}})))))))

(deftest print-outlier-count-test
  (testing "print-outlier-count"
    (testing "prints all outliers when all present"
      (is (= ["Found 10 outliers in 100 samples (10.0 %)"
              "low-severe\t 1 (1.0000 %)"
              "low-mild\t 2 (2.0000 %)"
              "high-mild\t 3 (3.0000 %)"
              "high-severe\t 4 (4.0000 %)"]
             (trimmed-lines
              (with-out-str
                (report/print-outlier-count
                 100
                 {:outlier-counts
                  (analyze/outlier-count
                   1 2 3 4)}))))))
    (testing "prints only present outliers"
      (is (= ["Found 5 outliers in 100 samples (5.00 %)"
              "low-mild\t 2 (2.0000 %)"
              "high-mild\t 3 (3.0000 %)"]
             (trimmed-lines
              (with-out-str
                (report/print-outlier-count
                 100
                 {:outlier-counts
                  (analyze/outlier-count
                   0 2 3 0)}))))))))


(deftest outlier-effect-test
  (is (= :unaffected (report/outlier-effect 0.009)))
  (is (= :slight (report/outlier-effect 0.09)))
  (is (= :moderate (report/outlier-effect 0.49)))
  (is (= :severe (report/outlier-effect 0.51))))
