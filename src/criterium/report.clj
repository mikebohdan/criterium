(ns criterium.report
  (:require [criterium
             [format :as format]
             [metric :as metric]
             [util :as util]]))

(util/optional-require
  [criterium.chart :as chart :refer [histogram view]])


(defn print-stat [path {:keys [mean variance] :as stat}]
  (let [{:keys [dimension label]} (metric/metric-format path)
        [scale units] (format/scale dimension (first mean))]
    (println
      (format "%36s: %.3g ± %.3g %s"
              label
              (* scale (first mean))
              (* scale 3 (Math/sqrt (first variance)))
              units))))

(defn view-histogram [{:keys [samples batch-size stats] :as result} path]
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
                       :value-label (str "value [" units"]")}]
    (criterium.chart/view
      (criterium.chart/histogram
        vs
        chart-options))))

(defn print-stats [result {:keys [histogram] :as options}]
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
