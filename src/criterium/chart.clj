(ns criterium.chart
  "Optional charting.
  Requires com.hypirion/clj-xchart."
  (:require [com.hypirion.clj-xchart :as c]))


(defn xy-chart [xs ys]
  (c/xy-chart
    {"Expected rate" [xs ys]
     }
    {:x-axis {:title "Number of evals"
              :logarithmic? true}
     :y-axis {:title "Elapsed"
              :logarithmic? true}}))

(defn bin-definition [mn mx n {:keys [delta num-bins]}]
  (cond
    (and (nil? delta) (nil? num-bins))
    (let [n-bins (max 5 (min n (quot n 10) 50))]
      {:n-bins n-bins
       :delta  (/ (- mx mn) (double n-bins)) })

    (nil? delta)
    (let [n-bins num-bins]
      {:n-bins n-bins
       :delta  (/ (- mx mn) (double n-bins)) })

    (nil? num-bins)
    (let [n-bins (quot (- mx mn) delta)]
      {:n-bins n-bins
       :delta  delta})

    :else
    {:n-bins num-bins
     :delta (double delta)}))


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
        tick-spacing (* delta (quot n-bins 5))]
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
      {"Histogram" {:x xs
                    :y ys}}
      {:legend {:position :inside-ne}
       :x-axis {:title "Value"
                :label {:rotation 90}
                :tick-mark-spacing-hint tick-spacing
                :decimal-pattern "###.#"}
       :y-axis {:title "Frequency"}})
    ))


(defn view [& charts]
  (apply c/view charts)
  nil)

(defn spit [chart path]
  (c/spit chart path))

(let [chart (histogram [1 2 2 3 3 3 4 4 5 5 6 8] {})]
  (view chart))
