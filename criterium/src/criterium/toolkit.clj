(ns criterium.toolkit
  "Functions used to implement benchmark measurements."
  (:require
   [criterium.budget :as budget]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.pipeline :as pipeline])
  (:import
   [criterium.budget Budget]))

(set! *unchecked-math* :warn-on-boxed)

(def ^Long NANOSEC-NS 1)
(def ^Long MICROSEC-NS 1000)
(def ^Long MILLISEC-NS 1000000)
(def ^Long SEC-NS 1000000000)

;;; Memory management

(defn force-gc
  "Force garbage collection and finalisers so that execution time
  associated with this is not incurred at another time. Up to
  max-attempts are run to clear all pending finalizers and free as much
  memory as possible.

  Returns samples with GC execution time, total changes in memory, and
  in object finalizers pending."
  [^long max-attempts]
  (let [measured (measured/expr (jvm/run-finalization-and-force-gc))
        pipeline (pipeline/with-memory
                   (pipeline/with-finalization-count
                     pipeline/time-metric))]
    (loop [samples  [] ; hold onto data we allocate here
           attempts 0]
      (let [sample                      (pipeline/execute pipeline measured 1)
            ^long new-memory-used       (-> sample :memory :total :used)
            ^long pending-finalisations (-> sample :finalization :pending)]
        (if (and (< attempts max-attempts)
                 (or (pos? pending-finalisations)
                     (< new-memory-used 0)))
          (recur (conj samples sample)
                 (inc attempts))
          (conj samples sample))))))


;; (defn with-force-gc
;;   "Pipeline function to Force garbage collection.
;;   Attempls GC up to max-attempt times before execution."
;;   [max-attempts next-fn]
;;   (fn [data measured]
;;     (-> data
;;         (force-gc max-attempts)
;;         (next-fn data measured))))


;;; Timing


(defn throw-away-sample
  "The initial evaluation is always un-representative.
  This function throws it away."
  [measured]
  (pipeline/execute pipeline/time-metric measured 1)
  nil)

(defn first-estimate
  "Run measured for an initial estimate of the time to execute.

  Returns an estimated execution time in ns."
  [measured]
  (let [pipeline pipeline/time-metric
        ;; The first evaluation is *always* unrepresentative
        _ignore  (pipeline/execute pipeline measured 1)
        s0       (pipeline/execute pipeline measured 1)]
    (pipeline/elapsed-time s0)))

(defn estimate-eval-count
  "Given an estimated execution time and a budget, estimate eval count."
  ^long [t0
         ^Budget budget
         ^long batch-time-ns]
  (let [t0        (long t0)
        n-est     (long (quot (.elapsed-time-ns budget) t0))
        n-avail   (min (.eval-count budget) n-est)
        n-desired (max 2 (long (quot batch-time-ns t0)))]
    (cond
      (<= n-avail 2)
      2

      (< n-avail (* 10 n-desired))
      (max 2 (quot n-avail 10))

      :else
      (loop [n-desired n-desired]
        ;; reduce to less than a predicted 1000 batches
        (if (or (< (quot (.elapsed-time-ns budget) (* t0 n-desired)) 1000)
                (< (quot (.eval-count budget) n-desired) 1000))
          n-desired
          (recur (* n-desired 10)))))))

(defn estimate-batch-size
  "Estimate batch-size for the given budget and batch execution-time."
  [t0
   budget
   batch-time-ns]
  (let [est-eval-count (estimate-eval-count
                        t0
                        budget
                        batch-time-ns)]
    est-eval-count))


;; (defn warmup-params
;;   "Estimate budget and batch-size for warmup"
;;   [measured
;;    t0
;;    budget
;;    warmup-fraction
;;    {:keys [warmup-period-ns]
;;     :as   options}]
;;   (let [warmup-budget        (budget/phase-budget
;;                               budget warmup-period-ns warmup-fraction default-fraction)
;;         ^long est-eval-count (estimate-eval-count
;;                                t0
;;                                (merge
;;                                  warmup-budget
;;                                  (select-keys options [:target-batch-time-ns])))
;;         batch-size           (max 1 (long (quot est-eval-count
;;                                           ^long (:eval-count measured))))]
;;     [warmup-budget batch-size]))


(defn sample
  "Sample measured for the given budget and batch size."
  [pipeline
   measured
   budget
   batch-size]
  {:pre [(measured/measured? measured)]}
  (loop [eval-count      0
         elapsed-time-ns 0
         samples         []]
    (let [sample          (pipeline/execute pipeline measured batch-size)
          t               (pipeline/elapsed-time sample)
          elapsed-time-ns (unchecked-add elapsed-time-ns t)
          eval-count      (unchecked-add eval-count (long (:eval-count sample)))]
      (if (or (budget/budget-remaining? budget elapsed-time-ns eval-count)
              (< (count samples) 2))
        (recur eval-count
               elapsed-time-ns
               (conj samples sample))
        {:eval-count      eval-count
         :elapsed-time-ns elapsed-time-ns
         :samples         (conj samples sample)
         :batch-size      batch-size}))))

(defn execution-time-from-batch
  ^long [^long elapsed-time-ns ^long eval-count]
  (max 1 (long (quot elapsed-time-ns eval-count))))

(defn estimate-execution-time
  "Return an initial estimate for the execution time of a measured function.

  Repeatedly times the invocation of the function and returns the
  minimum invocation time.

  For quick functions limit execution count, while for slower functions
  limit total execution time. Limit evaluations to eval-budget, or
  elapsed time to time-budget-ns."
  ^long [measured budget batch-size]
  (let [pipeline          pipeline/time-metric
        {:keys [elapsed-time-ns eval-count _samples]}
        (sample
         pipeline
         measured
         budget
         batch-size)
        ;; ^long n           (:eval-count (first samples))
        ]
    ;; (long (quot ^long (reduce min (map pipeline/elapsed-time samples))
    ;;             n))
    (execution-time-from-batch elapsed-time-ns eval-count)))

(defn warmup
  "Run measured for the given amount of time to enable JIT compilation.

  Returns a vector of execution count, deltas and estimated function
  execution time."
  [measured
   budget
   batch-size]
  (let [pipeline       (pipeline/with-class-loader-counts
                         (pipeline/with-compilation-time
                           pipeline/time-metric))]
    (loop [eval-count       0
           elapsed-time-ns  0
           delta-free-iters 0
           samples          []]
      (let [sample          (pipeline/execute pipeline measured batch-size)
            t               (pipeline/elapsed-time sample)
            elapsed-time-ns (unchecked-add elapsed-time-ns t)
            eval-count      (unchecked-add eval-count (long (:eval-count sample)))
            ^long cl-counts (-> sample :class-loader :loaded-count)
            ^long comp-time (-> sample :compilation :compilation-time)]
        (if (and (> delta-free-iters 2)
                 (not (budget/budget-remaining? budget elapsed-time-ns eval-count)))
          {:eval-count      eval-count
           :elapsed-time-ns elapsed-time-ns
           :samples         (conj samples sample)
           :batch-size      batch-size}
          (recur eval-count
                 elapsed-time-ns
                 (if (and (zero? cl-counts)
                          (zero? comp-time))
                   (unchecked-inc delta-free-iters)
                   0)
                 (conj samples sample)))))))
