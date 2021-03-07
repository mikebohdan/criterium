(ns criterium.analyze
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as sgen]
   [criterium.metric :as metric]
   [criterium.sampled-stats :as sampled-stats]
   [criterium.stats :as stats]
   [criterium.util :as util]))

(defmulti analyze-samples
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [analysis [result samples] metrics]
    (:analysis-type analysis)))

(defn exp [v]
  (Math/exp v))

(defmethod analyze-samples :transform-log
  ;; take the log of sampled values
  [_analysis [result samples] metrics]
  (reduce
   (fn [[result samples] metric]
     (reduce
      (fn [[result samples] path]
        [(util/add-transform result path exp)
         (mapv
          (fn [sample]
            (let [v (get-in sample path)]
              (if (pos? v)
                (merge
                 {}
                 sample
                 (assoc-in {} path (Math/log v)))
                sample)))
          samples)])
      [result samples]
      (metric/metric-paths metric)))
   [result samples]
   metrics))

(defmethod analyze-samples :samples
  ;; add samples to the result
  [_analysis [result samples] _metrics]
  [(assoc result :samples samples) samples])

(defmethod analyze-samples :stats
  ;; add stats to the result
  [analysis [result samples] metrics]
  (let [res (sampled-stats/sample-stats
             result
             metrics
             (:batch-size result)
             samples
             analysis)
        res (assoc
             res
             :jvm-event-stats
             (sampled-stats/jvm-event-stats samples))]
    [(assoc result :stats res) samples]))

(defmethod analyze-samples :bootstrap-stats
  ;; add stats to the result
  [analysis [result samples] metrics]
  (let [res (sampled-stats/bootstrap-stats
             result
             metrics
             (:batch-size result)
             samples
             analysis)
        res (assoc
             res
             :jvm-event-stats
             (sampled-stats/jvm-event-stats samples))]
    [(assoc result :bootstrap-stats res) samples]))

(defrecord OutlierCount [low-severe low-mild high-mild high-severe])

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  (->OutlierCount low-severe low-mild high-mild high-severe))

(defn add-outlier [low-severe low-mild high-mild high-severe counts x]
  (if (<= low-mild x high-mild)
    counts
    (outlier-count
     (if (<= x low-severe)
       (inc (:low-severe counts))
       (:low-severe counts))
     (if (< low-severe x low-mild)
       (inc (:low-mild counts))
       (:low-mild counts))
     (if (> high-severe x high-mild)
       (inc (:high-mild counts))
       (:high-mild counts))
     (if (>= x high-severe)
       (inc (:high-severe counts))
       (:high-severe counts)))))

(defn point-estimate [estimate]
  (first estimate))

(defn point-estimate-ci [estimate]
  (last estimate))

(defn outlier-significance
  "Find the significance of outliers given mean and variance estimates.
  Based on how well a gaussian can describe the sample stats.
  See http://www.ellipticgroup.com/misc/article_supplement.pdf, p17."
  [mean variance batch-size]
  (if (or (zero? variance) (< batch-size 16))
    0
    (let [variance-block (* batch-size variance)
          std-dev-block  (Math/sqrt variance-block)
          mean-g-min     (/ mean 2)
          sigma-g        (min (/ mean-g-min 4)
                              (/ std-dev-block (Math/sqrt batch-size)))
          variance-g     (* sigma-g sigma-g)
          batch-size-sqr (stats/sqr batch-size)
          c-max-f        (fn [t-min]    ; Eq 38
                           (let [j0-sqr (stats/sqr (- mean t-min))
                                 k0     (- (* batch-size-sqr j0-sqr))
                                 k1     (+ variance-block
                                           (- (* batch-size variance-g))
                                           (* batch-size j0-sqr))
                                 det    (- (* k1 k1)
                                           (* 4 variance-g k0))]
                             (Math/floor (/ (* -2 k0)
                                            (+ k1 (Math/sqrt det))))))
          var-out        (fn [c]        ; Eq 45
                           (let [nmc (- batch-size c)]
                             (* (/ nmc batch-size)
                                (- variance-block (* nmc variance-g)))))
          min-f          (fn [f q r]
                           (min (f q) (f r)))
          c-max          (min-f c-max-f 0 mean-g-min)]
      (/ (min-f var-out 1 c-max) variance-block))))

(defmethod analyze-samples :outlier-counts
  ;; add outliers to the results
  [_analysis [{:keys [batch-size] :as result} samples] metrics]
  (let [stats (:stats result)]
    (when-not stats
      (throw (ex-info "outlier analysis requires stats analysis" {})))
    [(reduce
      (fn [result metric]
        (reduce
         (fn [result path]
           (let [stat           (get-in (:stats stats) path)
                 thresholds     (stats/boxplot-outlier-thresholds
                                 (:0.25 stat)
                                 (:0.75 stat))
                 outlier-counts (reduce
                                 (apply partial add-outlier
                                        (mapv #(* batch-size %) thresholds))
                                 (outlier-count 0 0 0 0)
                                 (mapv #(get-in % path) samples))
                 outlier-sig    (when (some pos? (vals outlier-counts))
                                  (outlier-significance
                                   (:mean stat)
                                   (:variance stat)
                                   batch-size))]
             (update-in result (into [:stats :stats] path)
                        assoc
                        :outlier-counts outlier-counts
                        :outlier-significance outlier-sig)))
         result
         (metric/metric-paths metric)))
      result
      metrics)
     samples]))

(defn analyze
  [{:keys [samples] :as sampled} metrics analysis-config]
  (first
   (reduce
    #(analyze-samples %2 %1 metrics)
    [(-> sampled
         (dissoc :samples)
         (assoc :expr-value (:expr-value (first samples))))
     samples]
    analysis-config)))

(s/def ::tail-quantile
  (s/with-gen
    (s/and number? pos? #(< % 1))
    (sgen/double* {:min 0 :max 1})))

(s/def ::bootstrap-size nat-int?)

(s/def ::stats-config
  (s/keys :req-un [::analysis-type
                   ::tail-quantile
                   ::bootstrap-size]))

(s/def ::samples-config
  (s/keys :req-un [::analysis-type]))

(s/def ::analysis-config-map
  (s/or :stats ::stats-config
        :samples ::samples-config))

(s/def ::analysis-config
  (s/coll-of ::analysis-config-map))
