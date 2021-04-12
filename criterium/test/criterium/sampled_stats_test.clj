(ns criterium.sampled-stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.sampled-stats :as sampled-stats]
   [criterium.stats :as stats]
   [criterium.test-utils :refer [abs-error gen-bounded test-max-error]]
   [criterium.well :as well]
   [criterium.ziggurat :as ziggurat]))

(defn batch-transforms [batch-size]
  {:sample-> (list (fn [v] (/ v batch-size)))
   :->sample [(fn [v] (* v batch-size))]})

(def identity-transforms
  {:sample-> (list identity)
   :->sample [identity]})

(deftest stats-for-test
  (let [samples (mapv #(hash-map :v %) (repeat 100 1))
        stats   (sampled-stats/stats-for
                 [:v] samples {:tail-quantile 0.05} identity-transforms)]
    (is (= 1.0 (-> stats :mean)))
    (is (= 0.0 (-> stats :variance))))

  (testing "stats on [0..100]"
    (let [samples (mapv #(hash-map :v %) (range 101))
          stats   (sampled-stats/stats-for
                   [:v] samples {:tail-quantile 0.05} identity-transforms)]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance)))))

  (testing "stats on (reverse [0..100])"
    (let [samples (mapv #(hash-map :v %) (reverse (range 101)))
          stats   (sampled-stats/stats-for
                   [:v] samples {:tail-quantile 0.05} identity-transforms)]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance)))))

  (testing "stats on [0..100]*2 (ie batch-size 2)"
    (let [samples    (mapv #(hash-map :v %) (range 0 202 2))
          transforms (batch-transforms 2)
          stats      (sampled-stats/stats-for
                      [:v] samples {:tail-quantile 0.05} transforms)]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 1717.0 (-> stats :variance))))))

(deftest bootstrap-stats-for-test
  (testing "constant input"
    (let [samples (mapv #(hash-map :v %) (repeat 100 1))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v]
                   1
                   samples
                   {:tail-quantile 0.05}
                   identity-transforms)
          result  (stats/->BcaEstimate
                   1.0
                   [{:value 1.0 :alpha 0.05}
                    {:value 1.0 :alpha 0.95}])]
      (is (= result (-> stats :mean)))
      (is (= result (-> stats :0.25)))
      (is (= result (-> stats :0.75)))
      (is (= result (-> stats :lower-tail)))
      (is (= result (-> stats :upper-tail)))
      (is (= (stats/->BcaEstimate
              0.0
              [{:value 0.0 :alpha 0.05}
               {:value 0.0 :alpha 0.95}])
             (-> stats :variance)))))

  (testing "sequential input"
    (let [samples (mapv #(hash-map :v %) (range 101))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {:tail-quantile 0.05} identity-transforms)]
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u)))))

  (testing "reverse sequential input"
    (let [samples (mapv #(hash-map :v %) (reverse (range 101)))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {:tail-quantile 0.05} identity-transforms)]
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [{m                       :point-estimate
             [{l :value} {u :value}] :estimate-quantiles}
            (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u))))))


(deftest stats-for-test-property-1
  (let [batch-size   10000
        num-samples  100
        values       (take
                      (* batch-size num-samples)
                      (ziggurat/random-normal-zig
                       (well/well-rng-1024a)))
        sample-vals  (partition batch-size values)
        samples      (mapv #(hash-map :v (stats/sum %)) sample-vals)
        transforms   (batch-transforms batch-size)
        stats        (sampled-stats/stats-for
                      [:v] samples {:tail-quantile 0.05} transforms)
        mean-hat     (-> stats :mean)
        variance-hat (-> stats :variance)
        mean         (stats/mean values)
        variance     (stats/variance values)]
    (test-max-error mean mean-hat 1e-5)
    (test-max-error variance variance-hat 0.35)))

(defn random-values
  [random-seed mean n-sigma]
  (let [random-source (java.util.Random. random-seed)]
    (->> #(.nextDouble random-source)
         repeatedly
         ziggurat/random-normal-zig
         (map #(+ mean (* n-sigma %))))))

(comment
  (defspec random-values-test-property 10
    (prop/for-all
     [random-seed gen/large-integer
      mean (gen/double* {:min 0 :max 20 :infinite? false :NaN? false})
      sigma (gen/double* {:min 1 :max 1000 :infinite? false :NaN? false})]
     (let [values         (take 10000 (random-values random-seed mean sigma))
           variance       (* sigma sigma)
           mean-error     (abs-error (stats/mean values) mean)
           variance-error (abs-error (stats/variance values) variance)
           mean-tol       (max (* sigma 5e-2) 1e-2)
           variance-tol   (* variance 2e-1)]
       (is (< mean-error mean-tol) "mean")
       (is (< variance-error variance-tol) "variance")
       (and (< mean-error mean-tol)
            (< variance-error variance-tol))))))

(defn sample-values
  "Generate batched samples with mean 10 and std-dev 3"
  [batch-size num-samples random-seed mean sigma]
  (let [values      (->> (random-values random-seed mean sigma)
                         (take (* batch-size num-samples))
                         vec)
        sample-vals (partition batch-size values)
        samples     (mapv #(hash-map :v (stats/sum %)) sample-vals)]
    {:samples samples
     :values  values}))

(comment
  (defspec sample-values-test-property 10
    (prop/for-all
     [batch-size (gen-bounded 1 1)
      random-seed gen/pos-int
      mean (gen/double* {:min 0 :max 20 :infinite? false :NaN? false})
      sigma (gen/double* {:min 1 :max 1000 :infinite? false :NaN? false})]
     (let [num-samples    10000
           {:keys [samples values]}
           (sample-values batch-size num-samples random-seed mean sigma)
           mean-error     (abs-error (stats/mean values) mean)
           variance       (* sigma sigma)
           variance-error (abs-error (stats/variance values) variance)
           mean-tol       (max (* sigma 1e-1) 1e-2)
           variance-tol   (* variance 2e-1)]
       (is (< mean-error mean-tol) "mean")
       (is (< variance-error variance-tol) "variance")
       (and (< mean-error mean-tol)
            (< variance-error variance-tol))))))

(defn stats-values [batch-size num-samples random-seed mean sigma]
  (let [{:keys [samples
                values]} (sample-values batch-size num-samples random-seed mean sigma)
        transforms       (batch-transforms batch-size)
        stats            (sampled-stats/stats-for
                          [:v] samples {:tail-quantile 0.05} transforms)
        mean-hat         (-> stats :mean)
        variance-hat     (-> stats :variance)
        mean             (stats/mean values)
        variance         (* (stats/variance values) 1)]
    {:mean         mean
     :variance     variance
     :mean-hat     mean-hat
     :variance-hat variance-hat
     :samples      samples}))

(defspec stats-for-test-property 10
  (prop/for-all
      [batch-size (gen-bounded 1 1000)
       random-seed gen/pos-int]
    (let [num-samples    (long (quot 10000 batch-size))
          mean           10
          sigma          3
          {:keys [mean variance mean-hat variance-hat]}
          (stats-values batch-size num-samples random-seed mean sigma)
          mean-error     (abs-error mean mean-hat)
          variance-error (abs-error variance variance-hat)
          mean-tol       (max (* sigma 1e-1) 1e-2)
          variance-tol   (* variance 5e-1)]
      (is (< mean-error mean-tol))
      (is (< variance-error variance-tol))
      (and (< mean-error mean-tol)
           (< variance-error variance-tol)))))
