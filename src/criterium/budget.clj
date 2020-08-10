(ns criterium.budget
  (:require [clojure.spec.alpha :as s]
            [criterium
             [domain :as domain]
             [format :as format]]))

;;; Budget

(deftype Budget
    ;; use deftype so we cna have long values
    [^long elapsed-time-ns
     ^long eval-count])

(defn budget?
  "Predicate for x being a Budget"
  [x]
  (instance? Budget x))

(defn budget
  "Return a budget given possibly nil values for the specification"
  [^long elapsed-time-ns ^long eval-count]
  {:pre [(s/valid? ::domain/elapsed-time-ns elapsed-time-ns)
         (s/valid? ::domain/eval-count elapsed-time-ns)]}
  (->Budget
    (or elapsed-time-ns Long/MAX_VALUE)
    (or eval-count Long/MAX_VALUE)))

(defn budget*
  [[elapsed-time-ns eval-count]]
  (budget elapsed-time-ns eval-count))

(defn add
  ^Budget [& budgets]
  (reduce
    (fn [^Budget b ^Budget b1]
      (budget
        (+ (.elapsed-time-ns b) (.elapsed-time-ns b1))
        (+ (.eval-count b) (.eval-count b1))))
    (first budgets)
    (rest budgets)))

(defn subtract
  ^Budget [from-budget & other-budgets]
  (reduce
    (fn [^Budget b ^Budget b1]
      (budget
        (- (.elapsed-time-ns b) (.elapsed-time-ns b1))
        (- (.eval-count b) (.eval-count b1))))
    from-budget
    other-budgets))

(defn phase-budget
  "Return a budget for a measurement phase, given a total budget.
  The phase-period-ns is an optional limit on the time budget,
  The phase fraction is a fraction to apply to the total budget."
  ^Budget [^Budget total-budget
           period-ns
           fraction
           ^double default-fraction]
  {:pre [default-fraction]}
  (budget
    (cond
      (and period-ns fraction)
      (min ^long period-ns
           (long (* (.elapsed-time-ns total-budget) ^double fraction)))

      period-ns period-ns
      fraction (long (* (.elapsed-time-ns total-budget) ^double fraction))

      :else (long (* (.elapsed-time-ns total-budget) default-fraction)))
    (cond
      fraction (long (* (.eval-count total-budget) ^double fraction))
      (nil? period-ns) (long (* (.eval-count total-budget) default-fraction))
      :else Long/MAX_VALUE)))

(defn budget-remaining?
  [^Budget budget ^long elapsed-time-ns ^long eval-count]
  (and (< elapsed-time-ns (.elapsed-time-ns budget))
       (< eval-count (.eval-count budget))))

(defmethod clojure.core/print-method Budget
  [^Budget budget ^java.io.Writer writer]
  (if *print-readably*
    (.write writer
            (str "#criterium/Budget["
                 (.elapsed-time-ns budget)
                 " " (.eval-count budget) "]"))
    (.write writer
            (str "Budget "
                 (format/format-value :time-ns (.elapsed-time-ns budget))
                 " "
                 (.eval-count budget)
                 " evals"))))

(defmethod clojure.pprint/simple-dispatch Budget
  [^Budget budget]
  (clojure.pprint/pprint {:elapsed-time-ns (.elapsed-time-ns budget)
                          :eval-count      (.eval-count budget)}))

(s/fdef budget?
  :args (s/cat :x any?)
  :ret boolean?
  :fn #(= (:ret %) (instance? Budget (-> % :args :x))))

(s/def ::budget (s/and budget?
                         (comp nat-int? (fn [^Budget b] (.elapsed-time-ns b)))
                         (comp nat-int? (fn [^Budget b] (.eval-count b)))))

(s/fdef budget
  :args (s/cat :elapsed-time-ns ::domain/elapsed-time-ns
               :eval-count ::domain/eval-count)
  :ret ::budget
  :fn (s/and #(= (.elapsed-time-ns (:ret %)) (-> % :args :elapsed-time-ns))
             #(= (.eval-count (:ret %)) (-> % :args :eval-count))))

(s/fdef budget*
  :args (s/cat :arg (s/tuple ::domain/elapsed-time-ns ::domain/eval-count))
  :ret ::budget
  :fn (s/and #(= (.elapsed-time-ns (:ret %)) (-> % :args :arg first))
             #(= (.eval-count (:ret %)) (-> % :args :arg second))))

(s/fdef add
  :args (s/and (s/cat :budgets (s/+ ::budget))
               #(< (reduce + (map
                               (fn [^Budget b] (double (.elapsed-time-ns b)))
                               (:budgets %)))
                   Long/MAX_VALUE)
               #(< (reduce + (map
                               (fn [^Budget b] (double (.eval-count b)))
                               (:budgets %)))
                   Long/MAX_VALUE))
  :ret ::budget
  :fn (s/and #(= (.elapsed-time-ns (:ret %))
                 (reduce + (map
                             (fn [^Budget b] (.elapsed-time-ns b))
                             (-> % :args :budgets))))
             #(= (.eval-count (:ret %))
                 (reduce + (map
                             (fn [^Budget b] (.eval-count b))
                             (-> % :args :budgets))))))


(s/fdef subtract
  :args (s/and (s/cat :budgets (s/+ ::budget))
               #(>= (reduce - (map
                               (fn [^Budget b] (double (.elapsed-time-ns b)))
                               (:budgets %)))
                   0)
               #(>= (reduce - (map
                               (fn [^Budget b] (double (.eval-count b)))
                               (:budgets %)))
                   0))
  :ret ::budget
  :fn (s/and #(= (.elapsed-time-ns (:ret %))
                 (reduce - (map
                             (fn [^Budget b] (.elapsed-time-ns b))
                             (-> % :args :budgets) )))
             #(= (.eval-count (:ret %))
                 (reduce - (map
                             (fn [^Budget b] (.eval-count b))
                             (-> % :args :budgets) )))))
