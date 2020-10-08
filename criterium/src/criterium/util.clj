(ns criterium.util)


(defn- safe-keys
  [m]
  (assert (or (map? m) (nil? m)) (pr-str m))
  {:pre [(or (map? m) (nil? m))]}
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


;; (defn missing-spec
;;   [[namespace sep as-sym sep refs]]
;;   (let [f (fn [namespace ref] `(intern '~namespace '~ref))]
;;     [namespace as-sym
;;      (condp = sep
;;        nil    nil
;;        :as    nil
;;        :refer (mapv (partial f namespace) refs))]))

(defn missing-spec
  [[namespace-sym & [_sep _as-sym _sep _refs :as args]]]
  (reduce
   (fn [res [sep val]]
     (assoc res sep val))
   {:namespace-sym namespace-sym}
   (partition 2 args)))

(defn unrefer
  [orig-ns refer-syms]
  (doseq [sym refer-syms]
    (ns-unmap orig-ns sym)))

(defn unload-ns
  "Remove currently aliased namespace."
  [ns-sym alias-sym]
  (ns-unalias *ns* alias-sym)
  (remove-ns ns-sym)
  (dosync
    (commute
      (var-get #'clojure.core/*loaded-libs*)
      disj
      ns-sym)))

(defn load-ns
  "Add loaded namespace."
  [ns-sym]
  (dosync
    (commute
     (var-get #'clojure.core/*loaded-libs*)
     conj
     ns-sym)))

(def optional-nses (volatile! #{}))

(defn has-optional-ns? [ns-sym]
  (contains? @optional-nses ns-sym))

(defn optional-require
  "Optionally require a namespace."
  [& specs]
  {:pre [(every? vector? specs)]}
  (let [defs    (mapv missing-spec specs)
        orig-ns (ns-name *ns*)]
    (doseq [{:keys [namespace-sym] as-sym :as refer-syms :refer} defs]
      (when (and (find-ns namespace-sym)
                 (ns-resolve namespace-sym 'stub))
        (unrefer orig-ns refer-syms)
        (unload-ns namespace-sym as-sym))
      (try
        (if as-sym
          (require [namespace-sym :as as-sym])
          (require namespace-sym))
        (vswap! optional-nses conj namespace-sym)
        (catch Exception _
          (in-ns namespace-sym)
          (intern namespace-sym 'stub true)
          (doseq [ref refer-syms]
            (intern namespace-sym ref))
          (in-ns orig-ns)
          (load-ns namespace-sym)
          (when as-sym
            (alias as-sym namespace-sym))
          (require [namespace-sym :as as-sym :refer refer-syms])
          nil)))))

;; (defmacro optional-require
;;   "Optionally require a namespace."
;;   [& specs]
;;   (let [defs    (mapv missing-spec specs)
;;         orig-ns (ns-name *ns*)]
;;     `(do
;;        ~@(for [[namespace as-sym missing-defs] defs]
;;            `(do
;;               (when (and (find-ns '~namespace)
;;                          (ns-resolve '~namespace 'stub))
;;                 (unload-ns '~namespace '~as-sym))
;;               (try
;;                 (require '[~namespace :as ~as-sym])
;;                 (vswap! optional-nses conj '~namespace)
;;                 true
;;                 (catch Exception _#
;;                   (ns ~namespace)
;;                   (intern '~namespace '~'stub true)
;;                   ~@missing-defs
;;                   (in-ns '~orig-ns)
;;                   (require
;;                     '[~namespace :as ~as-sym])
;;                   nil)
;;                 ))))))

(defn assert-optional-ns [ns-sym message]
  (when-not (has-optional-ns? ns-sym)
    (throw (ex-info message {:missing-ns ns-sym}))))

;; (comment
;;   (optional-require
;;     (constantly nil)
;;     [ab :as ab :refer [a b]])
;;   (optional-require
;;     (constantly nil)
;;     [criterium.stats :as stats :refer [well]]))
