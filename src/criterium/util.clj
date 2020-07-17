(ns criterium.util)


(defn- safe-keys [m]
  (dissoc m :state :expr-value))

(defn- merge-fn [op]
  (fn merge-fn-inner [a b]
    (if (or (map? a) (map? b))
      (merge-with
        merge-fn-inner
        (safe-keys a)
        (safe-keys b))
      (op a b))))

(defn diff [finish start]
  (merge-with (merge-fn -) finish start))


(defn sum
  ([] {})
  ([a b]
   (merge-with
     (merge-fn +)
     (safe-keys a)
     (safe-keys b))))

(defn divide-by
  [divisor]
  (fn [x] (/ x divisor)))

(defn sig-figs
  "Round a double to the given precision (number of significant digits)"
  [d precision]
  (let [p (Math/ceil (Math/log10 d))
        factor (Math/pow 10 (- precision p))]
    (/ (Math/round (* d factor)) factor)))

(defn round-decimal-places
  "Round a double to the given decimal places"
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn round-up-to
  "Round v up to nearest multiple of f"
  [v f]
  (* f (Math/ceil (/ v f))))

(defn round-down-to
  "Round v down to nearest multiple of f"
  [v f]
  (* f (Math/floor (/ v f))))
