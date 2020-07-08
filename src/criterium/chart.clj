(ns criterium.chart
  "Optional charting.
  Requires com.hypirion/clj-xchart."
  (:require [com.hypirion.clj-xchart :as c]))


(defn chart [xs ys]
  (c/xy-chart
    {"Expected rate" [xs ys]
     }
    {:x-axis {:title "Number of evals"
              :logarithmic? true}
     :y-axis {:title "Elapsed"
               :logarithmic? true}}))


(defn view [& charts]
  (apply c/view charts)
  nil)
