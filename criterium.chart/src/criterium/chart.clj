(ns criterium.chart
  "Optional charting.
  Requires com.hypirion/clj-xchart."
  (:refer-clojure :exclude [spit])
  (:require [com.hypirion.clj-xchart :as c]
            [criterium.format :as format]
            [criterium.pipeline :as pipeline]
            [criterium.util :as util]))

(defn xy-chart [xs ys]
  (c/xy-chart
   {"Expected rate" [xs ys]}
   {:x-axis {:title "Number of evals"
             :logarithmic? false}
    :y-axis {:title "Elapsed"
             :logarithmic? false}}))

(defn bin-definition [mn mx n {:keys [delta num-bins]}]
  (let [delta (when delta (double delta))
        mn    (double mn)
        mx    (double mx)]
    (cond
      (and (nil? delta) (nil? num-bins))
      (let [n-bins (max 5 (min n (quot n 10) 50))
            delta  (util/sig-figs
                    (/ (- mx mn) (double n-bins))
                    1)
            mn     (if (zero? delta)
                     mn
                     (util/round-down-to mn delta))
            mx     (if (zero? delta)
                     mx
                     (util/round-up-to mx delta))]
        {:n-bins n-bins
         :delta  (if (zero? delta) 1 delta)
         :mn     mn
         :mx     mx})

      (nil? delta)
      (let [n-bins num-bins
            delta  (util/sig-figs
                    (/ (- mx mn) (double n-bins))
                    1)
            mn     (util/round-down-to mn delta)
            mx     (util/round-up-to mx delta)]
        {:n-bins n-bins
         :delta  delta
         :mn     mn
         :mx     mx})

      (nil? num-bins)
      (let [n-bins (int (quot (- mx mn) delta))
            mn     (util/round-down-to mn delta)
            mx     (util/round-up-to mx delta)]
        {:n-bins n-bins
         :delta  delta
         :mn     mn
         :mx     mx})

      :else
      (let [mn (util/round-down-to mn delta)
            mx (util/round-up-to mx delta)]
        {:n-bins num-bins
         :delta  (double delta)
         :mn     mn
         :mx     mx}))))

(defn histogram [vs options]
  (let [mn     (reduce min vs)
        mx     (reduce max vs)
        n      (count vs)
        {:keys [n-bins delta]} (bin-definition mn mx n options)
        binned (mapv #(long (quot (- % mn) delta)) vs)
        freqs  (frequencies binned)
        is     (range (inc n-bins))
        xs     (mapv #(+ mn (* % delta)) is)
        ys     (mapv #(get freqs % 0) is)
        tick-spacing (util/sig-figs
                      (* delta (quot n-bins 5))
                      1)]
    ;; (clojure.pprint/pprint
    ;;   {:mn mn
    ;;    :mx mx
    ;;    :n n
    ;;    :n-bins n-bins
    ;;    :delta delta
    ;;    :binned binned
    ;;    :freqs freqs
    ;;    :is is
    ;;    :xs xs
    ;;    :ys ys
    ;;    :tick-spacing tick-spacing})
    (c/category-chart
     {(:title options "Histogram")
      {:x xs
       :y ys}}
     {:legend {:position :inside-ne}
      :x-axis {:title (:value-label options "Value")
               :label {:rotation 90}
               :tick-mark-spacing-hint tick-spacing
               :decimal-pattern "###.##"}
      :y-axis {:title "Frequency"}})))

(defn view [& charts]
  (apply c/view charts)
  nil)

(defn spit [chart path]
  (c/spit chart path))

(defn time-histogram
  [{:keys [samples stats batch-size] :as _res} & [options]]
  {:pre [samples]}
  (let [{[mean]     :mean
         [variance] :variance} (:elapsed-time-ns (:stats stats))
        [scale units]          (format/scale :ns mean)
        upper-limit            (+ mean (* 2 (Math/sqrt variance)))
        vs                     (->>
                                samples
                                (mapv pipeline/elapsed-time)
                                (mapv #(/ % (double batch-size)))
                                (filterv #(< % upper-limit))
                                (mapv #(* % scale)))
        num-outliers           (- (count samples) (count vs))
        _                      (println {:mean         mean
                                         :variance     variance
                                         :upper-limit  upper-limit
                                         :num-outliers num-outliers
                                         :n            (count vs)})
        chart                  (histogram
                                vs
                                (merge
                                 {:value-label (str "Time [" units "]")}
                                 options))]
    (println "Ignoring n outliers: " num-outliers)
    chart))
