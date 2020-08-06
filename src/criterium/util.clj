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


(defn missing-spec
  [missing-fn [namespace sep as-sym sep refs]]
  (let [f (fn [namespace ref] `(intern '~namespace '~ref ~missing-fn))]
    [namespace as-sym
     (condp = sep
       nil    nil
       :as    nil
       :refer (mapv (partial f namespace) refs))]))


(defn unload-ns [ns-sym alias-sym]
  (ns-unalias *ns* alias-sym)
  (remove-ns ns-sym)
  ;; no need to remove from *loaded-libs*, since never loaded
  ;; via require.
  ;; (dosync
  ;;   (commute
  ;;     (var-get #'clojure.core/*loaded-libs*)
  ;;     #(remove (fn [x] (= x ns-sym)) %)))
  )

(def optional-nses (volatile! #{}))

(defn has-optional-ns? [ns-sym]
  (contains? @optional-nses ns-sym))


(defmacro optional-require
  "Optionally require a namespace."
  [missing-fn & specs]
  (let [defs (mapv (partial missing-spec missing-fn) specs)
        orig-ns (ns-name *ns*)]
    `(do
       ~@(for [[namespace as-sym missing-defs] defs]
           `(do
              (when (and (find-ns '~namespace)
                         (ns-resolve '~as-sym 'stub))
                (unload-ns '~namespace '~as-sym))
              (try
                (require '[~namespace :as ~as-sym])
                (vswap! optional-nses conj '~namespace)
                true
                (catch Exception _#
                  (ns ~namespace)
                  (intern '~namespace '~'stub true)
                  ~@missing-defs
                  (in-ns '~orig-ns)
                  (require
                    '[~namespace :as ~as-sym])
                  nil)
                ))))))

(comment
  (optional-require
    (constantly nil)
    [ab :as ab :refer [a b]])
  (optional-require
    (constantly nil)
    [criterium.stats :as stats :refer [well]]))
