(ns criterium.measured-test
  (:require
   [clojure.spec.test.alpha]
   [clojure.test :refer [deftest is testing]]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

;; (alias 'stc 'clojure.spec.test.check)

;; (stest/instrument
;;  [`measured/measured
;;   `measured/invoke
;;   `measured/expr])

(clojure.spec.test.alpha/check
 [`measured/measured
  `measured/invoke
  `measured/expr])

;; ((gen/generate (s/gen :criterium.measured/state-fn)))
;; ((gen/generate (s/gen :criterium.measured/fn)) 'abc)
;; ((gen/generate (s/gen :criterium.measured/expr-fn)))

;; (gen/generate (s/gen :criterium.measured/measured))

;; (gen/generate (s/gen :criterium.measured/measured))


;; (gen/generate (clojure.spec.gen.alpha/tuple
;;                (s/gen :criterium.measured/state-fn)
;;                (s/gen :criterium.measured/fn)
;;                (s/gen :criterium.measured/expr-fn)))

(deftest measured-test
  (let [eval-count (volatile! 0)
        m (measured/measured
           (fn [] :arg)
           (fn [arg ^long n]
             (vswap! eval-count #(+ n ^long %))
             [::time [arg arg]])
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
    (let [call-count         (volatile! 0)
          f                  (fn [v] (vswap! call-count inc) v)
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
      (is (= 1 (second (measured/invoke vec-nth-m))))))
  (testing "accepts time-fn option"
    (let [invokes (volatile! 0)
          f       (fn []
                    (vswap! invokes inc)
                    (jvm/current-thread-cpu-time))
          m       (measured/expr 1 {:time-fn f})]
      (is (= 1 (second (measured/invoke m))))
      (is (= 2 @invokes)))))
