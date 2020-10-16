(ns criterium.analyze
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [criterium
             [output :as output]
             [sampled-stats :as sampled-stats]]))

(defmulti analyze-samples
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [analysis result samples metrics]
    (:analysis-type analysis)))

(defmethod analyze-samples :samples
  ;; mode to just return the samples
  [_analysis result samples _metrics]
  (assoc result :samples samples))

(defmethod analyze-samples :stats
  ;; mode to just return the samples
  [analysis result samples metrics]
  (output/progress "Num samples" (count samples))
  (output/progress "Batch size" (:batch-size result))
  (let [res (sampled-stats/sample-stats
             metrics
             (:batch-size result)
             samples
             analysis)
        res  (assoc
              res
              :jvm-event-stats
              (sampled-stats/jvm-event-stats samples))]
    (assoc result :stats res)))

(defn analyze
  [sampled metrics analysis-config]
  (reduce
   #(analyze-samples %2 %1 (:samples sampled) metrics)
   (dissoc sampled :samples)
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
