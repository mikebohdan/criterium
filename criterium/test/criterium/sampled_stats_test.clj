(ns criterium.sampled-stats-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.sampled-stats :as sampled-stats]
   [criterium.stats :as stats]
   [criterium.well :as well]
   [criterium.ziggurat :as ziggurat]))

(defmacro test-max-error [expected actual max-error]
  `(is (< (Math/abs (- ~expected ~actual)) ~max-error)))

(deftest stats-for-test
  (let [samples (mapv #(hash-map :v %) (repeat 100 1))
        stats   (sampled-stats/stats-for [:v] 1 samples {})]
    (is (= 1.0 (-> stats :mean)))
    (is (= 0.0 (-> stats :variance))))

  (testing "stats on [0..100]"
    (let [samples (mapv #(hash-map :v %) (range 101))
          stats   (sampled-stats/stats-for
                   [:v] 1 samples {:tail-quantile 0.05})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance)))))

  (testing "stats on (reverse [0..100])"
    (let [samples (mapv #(hash-map :v %) (reverse (range 101)))
          stats   (sampled-stats/stats-for
                   [:v] 1 samples {:tail-quantile 0.05})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance)))))

  (testing "stats on [0..100]*2"
    (let [samples (mapv #(hash-map :v %) (range 0 202 2))
          stats   (sampled-stats/stats-for
                   [:v] 2 samples {:tail-quantile 0.05})]
      (is (= 50.0 (-> stats :mean)))
      (is (= 25.0 (-> stats :0.25)))
      (is (= 75.0 (-> stats :0.75)))
      (is (= 5.0 (-> stats :lower-tail)))
      (is (= 95.0 (-> stats :upper-tail)))
      (is (= 858.5 (-> stats :variance))))))

(deftest bootstrap-stats-for-test
  (testing "constant input"
    (let [samples (mapv #(hash-map :v %) (repeat 100 1))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {:tail-quantile 0.05})]
      (is (= [1.0 [1.0 1.0]] (-> stats :mean)))
      (is (= [1.0 [1.0 1.0]] (-> stats :0.25)))
      (is (= [1.0 [1.0 1.0]] (-> stats :0.75)))
      (is (= [1.0 [1.0 1.0]] (-> stats :lower-tail)))
      (is (= [1.0 [1.0 1.0]] (-> stats :upper-tail)))
      (is (= [0.0 [0.0 0.0]] (-> stats :variance)))))

  (testing "sequential input"
    (let [samples (mapv #(hash-map :v %) (range 101))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {})]
      (let [[m [l u]] (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [[m [l u]] (-> stats :variance)]
        (is (< l m u))
        (is (< l 858.5 u)))))

  (testing "reverse sequential input"
    (let [samples (mapv #(hash-map :v %) (reverse (range 101)))
          stats   (sampled-stats/bootstrap-stats-for
                   [:v] 1 samples {})]
      (let [[m [l u]] (-> stats :mean)]
        (is (< l m u))
        (is (< l 50 u)))
      (let [[m [l u]] (-> stats :variance)]
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
        stats        (sampled-stats/stats-for [:v] batch-size samples {} [])
        mean-hat     (-> stats :mean)
        variance-hat (-> stats :variance)
        mean         (stats/mean values)
        variance     (stats/variance values)]
    (test-max-error mean mean-hat 1e-5)
    (test-max-error variance variance-hat 0.2)))


(defn gen-bounded
  "Generates a long in the range from min-val to max-val inclusive.
  Unlike gen/choose, this is bounded by the generator's `size` parameter,
  starting at min-val."
  [min-val max-val]
  (let [r (- max-val min-val)]
    (gen/fmap
     (fn [x] (+ min-val x))
     (gen/sized
      (fn [size]
        (let [s (min size r)]
          (gen/choose 0 s)))))))

(defn sample-values [batch-size num-samples random-seed]
  (let [random-source (java.util.Random. random-seed)
        values        (->> #(.nextDouble random-source)
                           repeatedly
                           ziggurat/random-normal-zig
                           (map #(+ 10.0 (* 3 %)))
                           (take (* batch-size num-samples)))
        sample-vals   (partition batch-size values)
        samples       (mapv #(hash-map :v (stats/sum %)) sample-vals)]
    {:samples samples
     :values  values}))

(defn stats-values [batch-size num-samples random-seed]
  (let [{:keys [samples
                values]} (sample-values batch-size num-samples random-seed)
        stats            (sampled-stats/stats-for [:v] batch-size samples {})
        mean-hat         (-> stats :mean)
        variance-hat     (-> stats :variance)
        mean             (stats/mean values)
        variance         (stats/variance values)]
    {:mean         mean
     :variance     variance
     :mean-hat     mean-hat
     :variance-hat variance-hat
     :samples      samples}))

(defspec stats-for-test-property
  (prop/for-all
   [batch-size (gen-bounded 10000 100000)
    num-samples (gen-bounded 10 1000)
    random-seed gen/pos-int]
   (let [{:keys [mean variance mean-hat variance-hat]}
         (stats-values batch-size num-samples random-seed)]
     (test-max-error mean mean-hat 1e-5)
     (test-max-error variance variance-hat 0.2))))
