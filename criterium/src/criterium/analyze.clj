(ns criterium.analyze
  (:require [criterium
             [output :as output]
             [sampled-stats :as sampled-stats]]))

(defmulti analyze-samples
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [process-mode sampled metrics options]
    process-mode))

(defmethod analyze-samples :samples
  ;; mode to just return the samples
  [_process-mode sampled _metrics _options]
  (assoc sampled :samples (:samples sampled)))

(defmethod analyze-samples :stats
  ;; mode to just return the samples
  [_process-mode sampled metrics options]
  (output/progress "Num samples" (count (:samples sampled)))
  (output/progress "Batch size" (:batch-size sampled))
  (let [res (sampled-stats/sample-stats
             metrics
             (:batch-size sampled)
             (:samples sampled)
             options)
        res  (assoc
              res
              :jvm-event-stats
              (sampled-stats/jvm-event-stats (:samples sampled)))]
    (assoc sampled :stats res)))


(defn pipeline-metrics
  "Return a sequence of all metrics produced by a pipeline with the
  given spec."
  [{:keys [pipeline] :as _options}]
  (conj (:stages pipeline) (:terminator pipeline)))

(defn analyze
  [sampled {:keys [analysis] :as options}]
  (let [metrics (pipeline-metrics options)]
    (reduce
     #(analyze-samples %2 %1 metrics options)
     sampled
     (:analysis options))))
