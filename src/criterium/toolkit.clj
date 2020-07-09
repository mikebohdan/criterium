;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.

(ns criterium.toolkit
  "Standalone instrumentation and measurement functions."
  (:require [criterium
             [eval :as eval]
             [jvm :as jvm]
             [util :as util]]
            [clojure.walk :as walk]))


(def NANOSEC-NS 1)
(def MICROSEC-NS 1000)
(def MILLISEC-NS 1000000)
(def SEC-NS 1000000000000)

(defrecord Measured
    [state-fn f])

(defn measured? [x]
  (instance? Measured x))

(defn measured
  "Return a Measured for the given state function and function to be measured.

  The basic unit of measurement, a Measured consists of a state generation funtion and a
  function to be measured.  The function to be measured takes the result of calling the
  state function as an argument, ie. (f (state-fn)).

  The call of the state function is not measured.

  This is used to prevent constant folding for constant inputs."
  [state-fn f]
  (->Measured state-fn f))

(defn- measured-expr*
  "Return a measured function for the given expression.
  The arguments are converted into a vector, which is used as an argument to the a
  function that wraps the expression.
  Any expr that is not a List is treated as a constant.  This is mainly for
  internal benchmarking."
  [expr]
  (if (list? expr)
    (let [args     (vec (drop 1 expr))
          arg-syms (vec (repeatedly (count args) (fn [] (gensym "arg"))))]
      `(measured
         (fn [] ~args)
         (fn [~arg-syms] (~(first expr) ~@arg-syms))))
    `(measured
       (fn [] ~expr)
       identity)))

(defmacro measured-expr
  "Return a Measured for the given expression."
  [expr]
  (measured-expr* expr))

;;; Instrumentation

(defn assoc-delta
  "Assoc finish merged with start using op, onto the :delta key."
  [{:keys [start finish] :as data}]
  (assoc data :delta (util/diff finish start)))


;; Functions to wrap a Neasured execution.  The functions use a first data
;; argument to accumulate data about the Measured evaluation.

(defn instrumented
  "Executes an instrumented measured.

  The expr contains nested with-* calls, and returns
  a map with the collected data.  The inner-most with- form
  must be either with-time, or with-expr-value.

  e.g, to collect execution time, and result value for executing
  some-expr, use:

     (toolkit/instrumented
       (toolkit/measured-expr some-expr)
       (toolkit/with-time))"
  [{:keys [state-fn f] :as measured} next-fn]
  (assert (measured? measured))
  (let [state (state-fn)
        data {:state state}]
    (next-fn data measured)))

(defn with-expr-value
  "Execute measured, adding the return value to the data map's :expr-value key."
  []
  (fn [{:keys [state] :as data} {:keys [f] :as _measured}]
    (assoc data :expr-value (f state) :num-evals {:delta 1})))

(defn with-time
  "Execute measured, adding timing to the data map.

  Adds maps to the :time key in data, with the :before, :after, and :delta sub-data.
  Each map contains the :elapsed key with a timestamp in nanoseconds."
  []
  (fn [{:keys [state] :as data} {:keys [f] :as _measured}]
    (let [start      (jvm/timestamp)
          expr-value (f state)
          finish     (jvm/timestamp)]
      (-> data
         (assoc :time {:start  {:elapsed start}
                       :finish {:elapsed finish}}
                :expr-value expr-value
                :num-evals {:delta 1})
         (update-in [:time] assoc-delta)))))


(defn with-class-loader-counts
  "Execute measured, adding class loading counts to the data map.

  Adds maps to the :class-loader key in data, with the :before, :after, and :delta
  sub-keys.  Each map contains the :loaded-count and :unloaded-count keys.

  Uses the ClassLoadingMXBean."
  [next-fn]
  (fn [data measured]
    (assert (measured? measured))
    (-> data
       (assoc-in [:class-loader :start] (jvm/class-loader-counts))
       (next-fn measured)
       (assoc-in [:class-loader :finish] (jvm/class-loader-counts))
       (update-in [:class-loader] assoc-delta))))


(defn with-compilation-time
  "Execute measured, add compilation time to the data map.

  Adds maps to the :compilation key in data, with the :start, :finish, and :delta
  sub-keys.  Each map contains the :compilation-time key.

  Uses the CompilationMXBean."
  [next-fn]
  (fn [data measured]
    (assert (measured? measured))
    (-> data
       (assoc-in [:compilation :start] (jvm/compilation-time))
       (next-fn measured)
       (assoc-in [:compilation :finish] (jvm/compilation-time))
       (update-in [:compilation] assoc-delta))))


(defn with-memory
  "Execute measured, add compilation time to the data map.

  Adds maps to the :memory key in data, with the :start, :finish, and :delta sub-keys.
  Each map contains sub-maps for each type of memory, and the total memory (on
  the :total key).  Each sub-map contains the :init, :committed, :max and :used keys.

  Uses the MemoryMXBean."
  [next-fn]
  (fn [data measured]
    (assert (measured? measured))
    (-> data
       (assoc-in [:memory :start] (jvm/memory))
       (next-fn measured)
       (assoc-in [:memory :finish] (jvm/memory))
       (update-in [:memory] assoc-delta))))


(defn with-runtime-memory
  "Execute measured, add runtime memory to the data map.

  Adds maps to the :runtime-memory key in data, with the :start, :finish,
  and :delta sub-keys.

  Uses the java Runtime class."
  [next-fn]
  (fn [data measured]
    (assert (measured? measured))
    (-> data
       (assoc-in [:runtime-memory :start] (jvm/runtime-memory))
       (next-fn measured)
       (assoc-in [:runtime-memory :finish] (jvm/runtime-memory))
       (update-in [:runtime-memory] assoc-delta))))


(defn with-finalization-count
  "Execute measured, add pending finalization count to the data map.

  Adds maps to the :finalization key in data, with the :start, :finish,
  and :delta sub-keys.

  Uses the MemoryMXBean."
  [next-fn]
  (fn [data measured]
    (assert (measured? measured))
    (-> data
       (assoc-in [:finalization :start] (jvm/finalization-count))
       (next-fn measured)
       (assoc-in [:finalization :finish] (jvm/finalization-count))
       (update-in [:finalization] assoc-delta))))


(defn with-garbage-collector-stats
  "Execute measured, add garbage collection counts and times to the data map.

  Uses the GarbageCollectorMXBean beans."
  [next-fn]
  (fn [data measured]
    (assert (measured? measured))
    (-> data
       (assoc-in [:garbage-collector :start] (jvm/garbage-collector-stats))
       (next-fn measured)
       (assoc-in [:garbage-collector :finish] (jvm/garbage-collector-stats))
       (update-in [:garbage-collector] assoc-delta))))

(def measures
  {:class-loader       with-class-loader-counts
   :compilation-time   with-compilation-time
   :memory             with-memory
   :runtime-memory     with-runtime-memory
   :finalization-count with-finalization-count
   :garbage-collector  with-garbage-collector-stats})

(defn pipeline
  "Return a pipeline for the given metrics."
  [measure-kws & {:keys [terminal-fn] :or {terminal-fn with-time}}]
  (reduce
    (fn [pipeline measure-kw]
      (let [f (measures measure-kw)]
        (f pipeline)))
    (terminal-fn)
    measure-kws))

(defn deltas
  "Return a data map containing only the delta data.

  Discards all :start and :finish values, and moves :delta
  values up a level in the map."
  [data]
  (reduce-kv
    (fn [data k v]
      (let [delta (and (map? v) (:delta v))]
        (cond
          delta (assoc data k delta)
          (#{:state :expr-value} k) data
          :else (assoc data k v))))
    (select-keys data [:state :expr-value])
    data))

(defn total
  "Sum measured values across samples."
  [samples]
  (reduce util/sum samples))

(defn divide
  "Divide values across samples."
  [data divisor]
  (walk/postwalk
    #(cond
       (integer? %) (quot % divisor)
       (number? %) (/ % divisor)
       :else %)
    data))

(defn sample
  "Sample by invoking measured using pipeline.
  Collects an instrumentation data map for each invocation."
  [measured
   pipeline
   {:keys [time-budget-ns
           eval-budget]
    :or   {time-budget-ns (* 500 MILLISEC-NS)
           eval-budget    1000000}
    :as _options}]
  (loop [result         []
         eval-budget    (long eval-budget)
         time-budget-ns (long time-budget-ns)]
    (if (and (pos? eval-budget) (pos? time-budget-ns))
      (let [vals   (instrumented measured pipeline)
            deltas (deltas vals)
            t      (long (-> deltas :time :elapsed))]
        (recur
          (conj result deltas)
          (unchecked-dec eval-budget)
          (unchecked-subtract time-budget-ns t)))
      result)))

;;; Memory management

(defn force-gc
  "Force garbage collection and finalisers so that execution time
  associated with this is not incurred at another time. Up to
  max-attempts are run to clear all pending finalizers and free as
  much memory as possible.

  Returns the GC execution time,  total changes in memory, and in
  object finalizers pending."
  [max-attempts]
  (let [measured (measured-expr (jvm/run-finalization-and-force-gc))
        pipeline (with-memory
                   (with-finalization-count
                     (with-time)))]
    (loop [all-deltas [] ; hold onto data we allocate here
           attempts   0]
      (let [deltas (deltas (instrumented measured pipeline))]
        (let [new-memory-used (-> deltas :memory :total :used)]
          (if (and (< attempts max-attempts)
                   (or (pos? (-> deltas :finalization :pending))
                       (< new-memory-used 0)))
            (recur (conj all-deltas deltas)
                   (inc attempts))
            (reduce util/sum all-deltas)))))))

(defn with-force-gc
  "Force garbage collection up to max-attempt times before execution."
  [max-attempts next-fn]
  (fn [data expr]
    (-> data
       (force-gc max-attempts)
       (next-fn measured))))

;;; timing
(defn elapsed-time [data]
  (-> data :time :elapsed))

(defn estimate-execution-time
  "Return an initial estimate for the execution time of a measured function.

  Repeatedly times the invocation of the function and returns the minimum invocation
  time.

  For quick functions limit execution count, while for slower functions limit total
  execution time. Limit evaluations to eval-budget, or elapsed time to time-budget-ns."
  [measured
   {:keys [time-budget-ns eval-budget]
    :or   {time-budget-ns (* 100 MILLISEC-NS)
           eval-budget    1000}
    :as   _options}]
  (let [pipeline     (with-time)
        measure-time (fn []
                       (-> (instrumented measured pipeline)
                          deltas
                          :time :elapsed))
        samples      (sample
                      measured
                      pipeline
                      {:time-budget-ns time-budget-ns
                       :eval-budget    eval-budget})
        elapsed      (map elapsed-time samples)
        min-t        (reduce min elapsed)
        sum          (total samples)
        sum-t        (elapsed-time sum)
        num-evals    (:num-evals sum)
        avg          (divide sum (:num-evals sum))
        avg-t        (elapsed-time avg)]
    {:min       min-t
     :sum       sum-t
     :avg       avg-t
     :num-evals num-evals}))
