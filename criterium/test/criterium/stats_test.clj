(ns criterium.stats-test
  (:require [clojure.test :refer [deftest is]]
            [criterium
             [stats :as stats]
             [well :as well]]))

(defmacro test-max-error [expected actual max-error]
  `(is (< (Math/abs (- ~expected ~actual)) ~max-error)))

(deftest mean-test
  (is (= 1 (stats/mean (take 20 (repeatedly (constantly 1))))))
  (is (= 3 (stats/mean (range 0 7)))))

(deftest sum-test
  (is (= 20 (stats/sum (take 20 (repeatedly (constantly 1))))))
  (is (= 21 (stats/sum (range 0 7)))))

(deftest sum-of-squares-test
  (is (= 20.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 1))))))
  (is (= 80.0 (stats/sum-of-squares (take 20 (repeatedly (constantly 2))))))
  (is (= 91.0 (stats/sum-of-squares (range 0 7)))))

(deftest variance-test
  (is (= 0.0 (stats/variance (take 20 (repeatedly (constantly 1))))))
  (is (= 4.0 (stats/variance (range 0 7) 0))))

(deftest median-test
  (is (= [5 [1 2] [7 8]]
         (stats/median [1 2 5 7 8])))
  (is (= [7/2 [1 2 2] [5 7 8]]
         (stats/median [1 2 2 5 7 8]))))

(deftest quartiles-test
  (is (= [3/2 5 15/2]
         (stats/quartiles [1 2 5 7 8])))
  (is (= [2 7/2 7]
         (stats/quartiles [1 2 2 5 7 8]))))

(deftest boxplot-outlier-thresholds-test
  (is (= [-4.0 -1.0 7.0 10.0] (stats/boxplot-outlier-thresholds 2.0 4.0))))

(deftest bootstrap-estimate-test
  (is (= [1 0.0 [1.0 1.0]]
         (stats/bootstrap-estimate (take 20 (repeatedly (constantly 1))))))
  (is (= [2 0.0 [2.0 2.0]]
         (stats/bootstrap-estimate (take 20 (repeatedly (constantly 2))))))
  ;; (is (= [1/2 0.26315789473684204 [-0.5054587850434509 1.5054587850434509]]
  ;;        (bootstrap-estimate (take 20 (cycle [0 1])))))
  (let [[m s [l u]] (stats/bootstrap-estimate (take 1000000 (repeatedly rand)))]
    (is (test-max-error 0.5 m 1e-2))
    (is (test-max-error 0.0 l 0.2))
    (is (test-max-error 1.0 u 0.2))
    (is (test-max-error 0.0833 s 0.2))))

#_(comment
    (let [f (fn [n] (take n (repeatedly rand)))]
      (dissoc (criterium.time/measure (f 1000000)) :expr-value))

    (let [f (fn [n] (take n (criterium.well/well-rng-1024a)))]
      (dissoc (criterium.time/measure (f 1000000)) :expr-value))

    (criterium.time/time (bootstrap-estimate (take 1000000 (repeatedly rand))))

    (let [f (fn [n] (bootstrap-estimate (take n (repeatedly rand))))]
      (double (/ (criterium.toolkit/elapsed-time
                  (-> (criterium.time/measure
                       (f 1000000))
                      (dissoc :expr-value)))
                 (double criterium.toolkit/MILLISEC-NS))))

    (def m (criterium.arg-gen/for-all
            [v (clojure.test.check.generators/vector
                (clojure.test.check.generators/double* {:inifinte? false :NaN? false
                                                        :min       0     :max  1})
                1000000)]
            (bootstrap-estimate v)))

    (dissoc (criterium.measure/measure m {}) :state))

(deftest bootstrap-estimate-scale-test
  (is (= [1e-9 [1e-8 1e-8]]
         (stats/scale-bootstrap-estimate [1 1 [10 10]] 1e-9))))

;; Values from R, qnorm (with options(digits=15))
(deftest normal-quantile-test
  (is (pos? (stats/normal-quantile 0.5001)))
  (is (neg? (stats/normal-quantile 0.4999)))
  (is (< 2e-8 (- (stats/normal-quantile 0.999) (stats/normal-quantile 0.001))))
  (let [max-error 1.0e-7]
    (is (= 0.0 (stats/normal-quantile 0.5)))
    (is (test-max-error 1.2815515655446 (stats/normal-quantile 0.9) max-error))
    (is (test-max-error 0.674489750196082 (stats/normal-quantile 0.75) max-error))
    (is (test-max-error -1.03643338949379 (stats/normal-quantile 0.15) max-error))
    (is (test-max-error -2.32634787404084 (stats/normal-quantile 0.01) max-error))))


;; Values from R, erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1


(deftest erf-test
  (let [max-error 1.5e-7]
    (test-max-error 0.999999984582742 (stats/erf 4) max-error)
    (test-max-error 0.995322265018953 (stats/erf 2) max-error)
    (test-max-error 0.842700792949715 (stats/erf 1) max-error)
    (test-max-error 0.112462916018285 (stats/erf 0.1) max-error)
    (test-max-error 0.0112834155558497 (stats/erf 0.01) max-error)))

;; Values from R, pnorm
(deftest normal-cdf-test
  (let [max-error 1.5e-7]
    (test-max-error 0.99865010196837 (stats/normal-cdf 3.0) max-error)
    (test-max-error 0.977249868051821 (stats/normal-cdf 2.0) max-error)
    (test-max-error 0.841344746068543 (stats/normal-cdf 1.0) max-error)
    (test-max-error 0.691462461274013 (stats/normal-cdf 0.5) max-error)
    (test-max-error 0.5 (stats/normal-cdf 0.0) max-error)
    (test-max-error 0.158655253931457 (stats/normal-cdf -1.0) max-error)
    (test-max-error 0.00134989803163009 (stats/normal-cdf -3.0) max-error)))

(deftest quantiles-test
  (let [max-error 1.5e-7]
    (test-max-error 1.0 (stats/quantile 0.5 [0 1 2]) max-error)
    (test-max-error 1.5 (stats/quantile 0.5 [0 1 2 3]) max-error)
    (test-max-error 1.0 (stats/quantile 0.25 [0 1 1.5 2 3]) max-error)
    (test-max-error 2.0 (stats/quantile 0.75 [0 1 1.5 2 3]) max-error))
  (is (= 5.0 (stats/quantile 0.05 (range 0 101))))
  (is (= 95.0 (stats/quantile 0.95 (range 0 101)))))

(deftest bootstrap-test
  (is (= [1 0.0 [1.0 1.0]]
         (stats/bootstrap (take 20 (repeatedly (constantly 1)))
                          stats/mean
                          100
                          well/well-rng-1024a)))
  (is (=  [[1 0.0 [1.0 1.0]] [0.0 0.0 [0.0 0.0]]]
          (stats/bootstrap (take 20 (repeatedly (constantly 1)))
                           (juxt stats/mean stats/variance)
                           100
                           well/well-rng-1024a))))

(deftest bootstrap-bca-test
  (let [ci 0.95]
    (is (= [1 [1 1]]
           (stats/bootstrap-bca (take 20 (repeatedly (constantly 1)))
                                stats/mean
                                100
                                [0.5 ci (- 1.0 ci)]
                                well/well-rng-1024a)))
    (is (=  [[1 [1 1]] [0.0 [0.0 0.0]]]
            (stats/bootstrap-bca (take 20 (repeatedly (constantly 1)))
                                 (juxt stats/mean stats/variance)
                                 100
                                 [0.5 ci (- 1.0 ci)]
                                 well/well-rng-1024a)))))
