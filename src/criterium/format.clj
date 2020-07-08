(ns criterium.format
  "Metric formatters")


(defmulti scale
  "Scale value with given dimensions keyword.
  Return a [scale units] tuple.
  scale is a multiplicative factor. units is a string."
  (fn [dimension value] dimension))

(defmethod scale :default
  [_ value]
  [1 ""])

(defmethod scale :time
  [_ value]
  (cond
    (> value 60)   [(/ 60) "min"]
    (< value 1e-6) [1e9 "ns"]
    (< value 1e-3) [1e6 "µs"]
    (< value 1)    [1e3 "ms"]
    :else          [1 "s"]))


(def ONE-KB 1024)
(def ONE-MB (* 1024 1024))
(def ONE-GB (* 1024 1024 1024))

(defmethod scale :memory
  [_ value]
  (cond
    (< value ONE-KB) [1 "bytes"]
    (< value ONE-MB) [(/ ONE-KB) "Kb"]
    (< value ONE-GB) [(/ ONE-MB) "Mb"]
    :else            [(/ ONE-GB) "Gb"]))

(defmulti format-value
  "Format value to 3 significant figures in an appropriate unit for the scale."
  (fn [dimension value] dimension))

(defmethod format-value :default
  [dimension value]
  (format "%d" value))

(defmethod format-value :time
  [dimension value]
  (let [[scale unit] (scale dimension value)]
    (format "%3.3g %s" (double (* scale value)) unit)))

(defmethod format-value :memory
  [dimension value]
  (let [[scale unit] (scale dimension value)]
    (format "%3.3f %s" (double (* scale value)) unit)))
