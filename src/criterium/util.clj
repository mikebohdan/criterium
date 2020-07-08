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
