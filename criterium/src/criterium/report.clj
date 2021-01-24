(ns criterium.report
  (:require
   [criterium.format :as format]
   [criterium.metric :as metric]
   [criterium.util :as util]))

(util/optional-require
 '[criterium.chart :as chart :refer [histogram view]])

(defn print-stat [path {:keys [mean variance] :as _stat}]
  (let [{:keys [dimension label]} (metric/metric-format path)
        [scale units] (format/scale dimension (first mean))]
    (println
     (format "%36s: %.3g ± %.3g %s"
             label
             (* scale (first mean))
             (* scale 3 (Math/sqrt (first variance)))
             units))))

(defn view-histogram [{:keys [samples batch-size stats] :as _result} path]
  (util/assert-optional-ns
   'criterium.chart
   "Please add com.hypirion/clj-xchart to the classpath to enable histograms")
  (let [stat (get-in stats path)
        mean (first (:mean stat))
        {:keys [dimension label]} (metric/metric-format path)
        [scale units] (format/scale dimension mean)
        vs (->>
            samples
            (mapv (metric/path-accessor path))
            (mapv #(/ % (double batch-size)))
            (mapv #(* % scale)))
        chart-options {:title label
                       :value-label (str "value [" units "]")}]
    (view
     (histogram
      vs
      chart-options))))

(defn print-stats [result {:keys [histogram] :as _options}]
  (doseq [metric (:metrics result)]
    (doseq [path (metric/metric-paths metric)]
      (let [stat (get-in (:stats result) path)]
        (print-stat path stat)
        (when histogram
          (view-histogram result path))))))

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
  [{:keys [jvm-event-stats] :as _result} _options]
  (let [{:keys [compilation garbage-collection]} jvm-event-stats]
    (print-jvm-event-stat "JIT compilation" compilation)
    (print-jvm-event-stat "Garbage collection" garbage-collection)))

(defn print-final-gc-warnings
  [{:keys [samples] :as _result} _options]
  (let [total (reduce + (map (comp :time :total :garbage-collection) samples))]
    (when (pos? total)
      (println "Final GC" (format/format-value :time-ns total)))))
