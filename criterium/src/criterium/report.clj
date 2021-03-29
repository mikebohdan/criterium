(ns criterium.report
  (:refer-clojure :exclude [spit])
  (:require
   [clojure.spec.alpha :as s]
   [criterium.format :as format]
   [criterium.metric :as metric]
   [criterium.util :as util]))

(util/optional-require
 '[criterium.chart :as chart :refer [histogram spit view]])

(defn print-stat
  [path
   {:keys  [mean
            mean-minus-3sigma
            mean-plus-3sigma
            variance]
    minval :min
    :as    _stat}]
  (let [{:keys [dimension label]} (metric/metric-format path)
        [scale units]             (format/scale dimension mean)]
    (println
     (format "%36s: %.3g %s  3σ [%.3g %.3g]  min %.3g"
             label
             (* scale mean)
             units
             (* scale mean-minus-3sigma)
             (* scale mean-plus-3sigma)
             (* scale minval)))))

(defn print-bootstrap-stat
  [path
   {:keys  [mean
            mean-minus-3sigma
            mean-plus-3sigma
            variance]
    minval :min
    :as    _stat}]
  (let [{:keys [dimension label]} (metric/metric-format path)
        [scale units]             (format/scale
                                   dimension
                                   (:point-estimate mean))
        min-quantiles             (:estimate-quantiles minval)
        quantiles                 (:estimate-quantiles mean)
        var-quantiles             (:estimate-quantiles variance)]
    (println
     (format "%36s: %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
             (str label " min")
             (* scale (:point-estimate minval))
             units
             (* scale (-> min-quantiles first :value))
             (* scale (-> min-quantiles second :value))
             (-> min-quantiles first :alpha)
             (-> min-quantiles second :alpha)))
    (println
     (format "%36s: %.3g %s CI [%.3g %.3g] (%.3f %.3f)"
             (str label " mean")
             (* scale (:point-estimate mean))
             units
             (* scale (-> quantiles first :value))
             (* scale (-> quantiles second :value))
             (-> quantiles first :alpha)
             (-> quantiles second :alpha)))
    (println
     (format "%36s: [%.3g %.3g] %s "
             (str label " 3σ")
             (* scale (:point-estimate mean-minus-3sigma))
             (* scale (:point-estimate mean-plus-3sigma))
             units))))

(defn print-outlier-count
  [num-samples
   {:keys [outlier-counts] :as _stat}]
  (let [values (vals outlier-counts)
        types  ["low-severe" "low-mild" "high-mild" "high-severe"]]
    (when (some pos? values)
      (let [sum (reduce + values)]
        (util/report "Found %d outliers in %d samples (%.3g %%)\n"
                     sum num-samples (* 100.0 (/ sum num-samples))))
      (doseq [[v c] (->> (interleave values types)
                         (partition 2)
                         (filter #(pos? (first %))))]
        (util/report
         "\t%s\t %d (%2.4f %%)\n"
         c v (* 100.0 (/ v num-samples)))))))

(defn outlier-effect
  "Return a keyword describing the effect of outliers on a point estimate."
  [var-out-min]
  (cond
    (< var-out-min 0.01) :unaffected
    (< var-out-min 0.1)  :slight
    (< var-out-min 0.5)  :moderate
    :else                :severe))

(defn print-outlier-significance
  [{:keys [outlier-counts
           outlier-significance]
    :as   _stat}]
  (let [values (vals outlier-counts)
        labels {:unaffected "unaffected"
                :slight     "slightly inflated"
                :moderate   "moderately inflated"
                :severe     "severely inflated"}]
    (when (some pos? values)
      (util/report " Variance contribution from outliers : %.3g %%"
                   (* outlier-significance 100.0))
      (util/report " Variance is %s by outliers\n"
                   (-> outlier-significance outlier-effect labels)))))

(defn get-transforms [result path]
  (get-in result (into [:value-transforms] path)))

(defn print-stats [config result]
  (doseq [metric (:metrics config)]
    (doseq [path (metric/metric-paths metric)]
      (let [stat       (get-in (:stats (:stats result)) path)
            transforms (get-transforms result path)]
        (print-stat path stat)))))

(defn print-bootstrap-stats [config result]
  (doseq [metric (:metrics config)]
    (doseq [path (metric/metric-paths metric)]
      (let [stat       (get-in (:bootstrap-stats (:bootstrap-stats result)) path)
            transforms (get-transforms result path)]
        (print-bootstrap-stat path stat)))))

(defn print-outlier-counts [config {:keys [stats] :as _result}]
  (doseq [metric (:metrics config)]
    (doseq [path (metric/metric-paths metric)]
      (let [stat (get-in (:stats stats) path)]
        (print-outlier-count (:num-samples stats) stat)
        (print-outlier-significance stat)))))

(defn view-histogram*
  [{:keys [file title]} path mean vs]
  (util/assert-optional-ns
   'criterium.chart
   "Please add criterium/chart to the classpath to enable histograms")
  (assert mean "must have a mean provided")
  (let [{:keys [dimension label]} (metric/metric-format path)
        [scale units]             (format/scale dimension mean)
        vs                        (mapv #(* % scale) vs)
        chart-options             {:title       (or title label)
                                   :value-label (str "value [" units "]")}
        chart                     (histogram
                                   vs
                                   chart-options)]
    (if file
      (spit chart file)
      (view chart))))

(defn view-histogram
  [histogram-config
   {:keys [samples batch-size bootstrap-stats stats] :as result} path]
  (util/assert-optional-ns
   'criterium.chart
   "Please add criterium/chart to the classpath to enable histograms")
  (let [stat           (get-in (:stats stats) path)
        bootstrap-stat (get-in (:bootstrap-stats bootstrap-stats)
                               path)
        mean           (or (:point-estimate (:mean bootstrap-stat))
                           (:mean stat))
        _              (assert mean
                               "histogram requires stats analysis")
        transforms     (util/get-transforms result path)
        vs             (->>
                        samples
                        (mapv (metric/path-accessor path))
                        (mapv #(util/transform-sample->
                                %
                                transforms)))]
    (view-histogram* histogram-config path mean vs)))


(defn view-sample-histogram
  [histogram-config
   {:keys [samples batch-size bootstrap-stats stats] :as result} path]
  (util/assert-optional-ns
   'criterium.chart
   "Please add criterium/chart to the classpath to enable histograms")
  (let [stat           (get-in (:stats stats) path)
        bootstrap-stat (get-in (:bootstrap-stats bootstrap-stats)
                               path)
        mean           (or (:point-estimate (:mean bootstrap-stat))
                           (:mean stat))
        _              (assert mean
                               "histogram requires stats analysis")
        transforms     (util/get-transforms result path)
        mean           (util/transform->sample mean transforms)
        vs             (->>
                        samples
                        (mapv (metric/path-accessor path)))]
    (view-histogram* histogram-config path mean vs)))

(defn print-metrics
  [metrics]
  (doseq [[k v] metrics]
    (when ((set (keys metric/metric-paths)) k)
      (print (format/format-metric k v)))))

(defn print-jvm-event-stat
  [title
   {:keys [total-time-ms sample-count] :as _stat}]
  (when (pos? total-time-ms)
    (println title "ran for"
             (format/format-value :time-ms total-time-ms)
             "affecting"
             sample-count
             "samples")))

(defn print-jvm-event-stats
  [{:keys [jvm-event-stats] :as _result}]
  (let [{:keys [compilation garbage-collection]} jvm-event-stats]
    (print-jvm-event-stat "JIT compilation" compilation)
    (print-jvm-event-stat "Garbage collection" garbage-collection)))

(defn print-final-gc-warnings
  [{:keys [samples] :as _result}]
  (let [total (reduce + (map (comp :time :total :garbage-collection) samples))]
    (when (pos? total)
      (println "Final GC" (format/format-value :time-ns total)))))

(defmulti report-impl
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [report result config]
    (assert (:report-type report))
    (:report-type report)))

(defmethod report-impl :metrics
  [_report result _config]
  (print-metrics result))

(defmethod report-impl :stats
  [_report result config]
  (print-stats config result))

(defmethod report-impl :bootstrap-stats
  [_report result config]
  (print-bootstrap-stats config result))

(defmethod report-impl :outlier-counts
  [_report result config]
  (print-outlier-counts config result))

(defmethod report-impl :histogram
  [report {:keys [samples] :as result} config]
  (doseq [metric (:metrics config)]
    (doseq [path (metric/metric-paths metric)]
      (view-histogram report result path))))

(defmethod report-impl :sample-histogram
  [report {:keys [samples] :as result} config]
  (doseq [metric (:metrics config)]
    (doseq [path (metric/metric-paths metric)]
      (view-sample-histogram report result path))))

(defmethod report-impl :jvm-event-stats
  [_report {:keys [samples] :as result} _config]
  (print-jvm-event-stats (:stats result)))

(defmethod report-impl :final-gc-warnings
  [_report {:keys [samples] :as result} _config]
  (print-final-gc-warnings (:stats result)))

(defmethod report-impl :samples
  [_report {:keys [samples] :as result} _config]
  (prn :samples samples))

(defn report
  [result {:keys [report] :as config}]
  (doseq [report-config report]
    (report-impl report-config result config)))

(s/def ::stats-config
  (s/keys :req-un [::report-type]))

(s/def ::jvm-event-stats-config
  (s/keys :req-un [::report-type]))

(s/def ::final-gc-warnings-config
  (s/keys :req-un [::report-type]))

(s/def ::histogram-config
  (s/keys :req-un [::report-type]))

(s/def ::report-config-map
  (s/or
   :stats ::stats-config
   :jvm-event-stats ::jvm-event-stats-config
   :final-gc-warnings ::final-gc-warnings-config
   :histogram ::histogram-config))

(s/def ::report-config
  (s/coll-of ::report-config-map))
