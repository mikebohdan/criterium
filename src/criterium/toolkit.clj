(ns criterium.toolkit
  (:require [criterium
             [jvm :as jvm]
             [measured :as measured]
             [pipeline :as pipeline]]))


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
  [max-attempts]
  (let [measured (measured/expr (jvm/run-finalization-and-force-gc))
        pipeline (pipeline/with-memory
                   (pipeline/with-finalization-count
                     pipeline/time-metric))]
    (loop [samples  [] ; hold onto data we allocate here
           attempts 0]
      (let [sample          (pipeline/execute pipeline measured)
            new-memory-used (-> sample :memory :total :used)]
        (if (and (< attempts max-attempts)
                 (or (pos? (-> sample :finalization :pending))
                     (< new-memory-used 0)))
          (recur (conj samples sample)
                 (inc attempts))
          samples)))))

(defn with-force-gc
  "Pipeline function to Force garbage collection.
  Attempls GC up to max-attempt times before execution."
  [max-attempts next-fn]
  (fn [data measured]
    (-> data
       (force-gc max-attempts)
       (next-fn data measured))))

(defn first-estimate
  "Run measured for an initial estimate of the time to execute..

  Returns an estimated execution time in ns."
  [measured]
  (let [pline   pipeline/time-metric
        ;; The first evaluation is *always* unrepresentative
        _ignore (pipeline/execute measured pline)
        v0      (pipeline/execute measured pline)]
    (long (quot (pipeline/elapsed-time v0) (:eval-count measured)))))

(defn estimate-eval-count
  "Estimate batch-size."
  [t0 {:keys [time-budget-ns eval-budget target-batch-time-ns]
       :or {target-batch-time-ns (* 10 MILLISEC-NS)}
       :as _options}]
  (let [n-est     (min (some-> time-budget-ns (quot t0) long) Long/MAX_VALUE)
        n-avail   (min (or eval-budget Long/MAX_VALUE)
                     n-est)
        n-desired (max 1 (quot target-batch-time-ns t0))]
    (cond
      (<= n-avail 1)
      1

      (< n-avail (* 10 n-desired))
      (max 1 (quot n-avail 10))

      :else
      n-desired)))

(defn phase-budget
  [time-budget-ns eval-budget phase-period-ns phase-fraction]
  {:time-budget-ns (min
                     (long (quot time-budget-ns phase-fraction))
                     (or phase-period-ns Long/MAX_VALUE))
   :eval-budget    (long (quot eval-budget phase-fraction))})

(defn warmup-params
  "Estimate parameters for warmup"
  [measured
   t0
   {:keys [time-budget-ns eval-budget warmup-period-ns warmup-fraction
           target-batch-time-ns]
    :or   {warmup-fraction 10}
    :as   options}]
  (let [warmup-budget  (phase-budget
                         time-budget-ns eval-budget warmup-period-ns warmup-fraction)
        est-eval-count (estimate-eval-count
                         t0
                         (merge
                           warmup-budget
                           (select-keys options [:target-batch-time-ns])))]
    (assoc warmup-budget
           :batch-size (max 1 (quot est-eval-count (:eval-count measured))))))

(defn warmup
  "Run measured for the given amount of time to enable JIT compilation.

  Returns a vector of execution count, deltas and estimated function
  execution time."
  [measured
   t0
   {:keys [time-budget-ns eval-budget warmup-period-ns warmup-fraction]
    :as   options}]
  (let [{:keys [time-budget-ns eval-budget batch-size]}
        (warmup-params measured t0 options)

        eval-budget    (long eval-budget)
        time-budget-ns (long time-budget-ns)
        pline          (pipeline/with-class-loader-counts
                         (pipeline/with-compilation-time
                           pipeline/time-metric))

        measured-batch (if (= 1 batch-size)
                         measured  ; re-use measured if possible to keep jit
                         (measured/batch
                           measured
                           batch-size))]
    (loop [num-evals  0
           time-ns    0
           delta-free 0
           vs         []]
      (let [v               (pipeline/execute measured-batch pline)
            t               (pipeline/elapsed-time v)
            time-ns         (unchecked-add time-ns t)
            num-evals       (unchecked-add num-evals (long (:num-evals v)))
            ^long cl-counts (-> v :class-loader :loaded-count)
            ^long comp-time (-> v :compilation :compilation-time)
            ]
        (if (and (> delta-free 2)
                 (or (>= num-evals ^long eval-budget)
                     (>= time-ns ^long time-budget-ns)))
          {:num-evals      num-evals
           :time-ns        time-ns
           :samples        (conj vs v)
           :measured-batch measured-batch}
          (recur num-evals
                 time-ns
                 (if (and (zero? cl-counts)
                          (zero? comp-time))
                   (unchecked-inc delta-free)
                   0)
                 (conj vs v)))))))


(defn estimate-execution-time
  "Return an initial estimate for the execution time of a measured function.

  Repeatedly times the invocation of the function and returns the
  minimum invocation time.

  For quick functions limit execution count, while for slower functions
  limit total execution time. Limit evaluations to eval-budget, or
  elapsed time to time-budget-ns."
  [{:keys [eval-count] :as measured}
   {:keys [time-budget-ns eval-budget]
    :or   {time-budget-ns (* 100 ^long MILLISEC-NS)
           eval-budget    1000}
    :as   _options}]
  (let [pipeline     pipeline/time-metric
        samples      (pipeline/sample
                      pipeline
                      measured
                      {:time-budget-ns time-budget-ns
                       :eval-budget    eval-budget})
        elapsed      (map pipeline/elapsed-time samples)
        min-t        (quot ^long (reduce min elapsed) ^long eval-count)
        sum          (pipeline/total samples)
        sum-t        (pipeline/elapsed-time sum)
        num-evals    (:num-evals sum)
        avg          (pipeline/divide sum (:num-evals sum))
        avg-t        (pipeline/elapsed-time avg)]
    {:min       min-t
     :sum       sum-t
     :avg       avg-t
     :num-evals num-evals}))


;;; Memory
