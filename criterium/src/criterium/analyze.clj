(ns criterium.analyze
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as sgen]
   [criterium.sampled-stats :as sampled-stats]
   [criterium.stats :as stats]))

(defmulti analyze-samples
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [analysis result samples metrics]
    (:analysis-type analysis)))

(defmethod analyze-samples :samples
  ;; add samples to the result
  [_analysis result samples _metrics]
  (assoc result :samples samples))

(defmethod analyze-samples :stats
  ;; add stats to the result
  [analysis result samples metrics]
  (let [res (sampled-stats/sample-stats
             metrics
             (:batch-size result)
             samples
             analysis)
        res (assoc
             res
             :jvm-event-stats
             (sampled-stats/jvm-event-stats samples))]
    (assoc result :stats res)))

(defmethod analyze-samples :bootstrap-stats
  ;; add stats to the result
  [analysis result samples metrics]
  (let [res (sampled-stats/bootstrap-stats
             metrics
             (:batch-size result)
             samples
             analysis)
        res (assoc
             res
             :jvm-event-stats
             (sampled-stats/jvm-event-stats samples))]
    (assoc result :bootstrap-stats res)))

(defmethod analyze-samples :outliers
  ;; add outliers to the results
  [analysis result samples metrics]
  (let [stats (:stats result)]
    (when-not stats
      (throw (ex-info "outlier analysis requires stats analysis" {})))
    (reduce
     (fn [result [metric stat]]
       (let [thresholds (stats/boxplot-outlier-thresholds
                         (:0.25 stat)
                         (:0.75 stat))])
       )
     result
     (:stats stats))))

(defn analyze
  [{:keys [samples] :as sampled} metrics analysis-config]
  (reduce
   #(analyze-samples %2 %1 samples metrics)
   (-> sampled
       (dissoc :samples)
       (assoc :expr-value (:expr-value (first samples))))
   analysis-config))

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
