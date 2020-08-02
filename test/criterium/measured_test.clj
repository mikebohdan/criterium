(ns criterium.measured-test
  (:require [criterium.measured :as measured]
            [clojure.test :refer [deftest is testing]]))


(deftest measured-test
  (let [eval-count (volatile! 0)
        m (measured/measured
            (fn [] :arg)
            (fn [arg n]
              (vswap! eval-count #(+ n %))
              [::time [arg arg]])
            1
            (fn [] ::symbolic))]
    (is (measured/measured? m))
    (is (= ::symbolic (measured/symbolic m)))
    (testing "invoke calls the function with one eval"
      (vreset! eval-count 0)
      (is (= [::time [:arg :arg]] (measured/invoke m)))
      (is (= 1 @eval-count)))
    (testing "invoke with eval-count calls the function with the eval count."
      (vreset! eval-count 0)
      (is (= [::time [:arg :arg]] (measured/invoke m 3)))
      (is (= 3 @eval-count)))))

(deftest expr-test
  (testing "nil expr"
    (let [nil-m (measured/expr nil)]
      (is (nil? (second (measured/invoke nil-m))))))
  (testing "const expr"
    (let [const-m (measured/expr ::const)]
      (is (= ::const (second (measured/invoke const-m))))))
  (testing "function call"
    (let [fncall-m (measured/expr (identity ::value))]
      (is (= ::value (second (measured/invoke fncall-m))))))
  (testing "recursive function call"
    (let [call-count (volatile! 0)
          f (fn [v] (vswap! call-count inc) v)
          recursive-fncall-m (measured/expr (f (f ::value)))]
      (is (= ::value (second (measured/invoke recursive-fncall-m))))
      (is (= 2 @call-count))))
  (testing "const expression is lifted"
    (let [const-expr-m (measured/expr (identity (+ 1 2)))]
      (is (= 3 (second (measured/invoke const-expr-m))))
      (is (= [3] ((:state-fn const-expr-m))))))
  (testing "args are type hinted"
    ;; if this gives a reflection warning then it should be treated as
    ;; an error.
    (let [vec-nth-m (measured/expr (.nth [0 1 3] 1))]
      (is (= 1 (second (measured/invoke vec-nth-m)))))))
