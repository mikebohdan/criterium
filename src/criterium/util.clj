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


;; Modified version of clojure.walk to preserve metadata
(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (cond
    (list? form)
    (outer (with-meta
             (apply list (map inner form))
             (meta form)))

    (instance? clojure.lang.IMapEntry form)
    (outer
      (clojure.lang.MapEntry/create
        (inner (key form)) (inner (val form))))

    (seq? form)
    (outer (with-meta
             (doall (map inner form))
             (meta form)))

    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))

    (coll? form)
    (outer (with-meta
             (into (empty form) (map inner form))
             (meta form)))
    :else        (outer form)))

(defn postwalk
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  {:added "1.1"}
  [f form]
  (walk (partial postwalk f) f form))
