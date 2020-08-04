(ns criterium.pipeline
  "A measured pipeline.

  A pipeline is a function that takes a sample state and a measured
  and returns an updated sample state.  It is usually called via the
  execute function.

  A pipeline can be composed via pipeline functions and a pipeline
  terminal function."
  (:require [criterium
             [jvm :as jvm]
             [measured :as measured]
             [util :as util]]
            [clojure.walk :as walk]))

;; Metrics

;; Pipeline functions to wrap a Neasured execution.

(defn time-metric
  "A terminal function to execute measured, adding results to the data map.

  Puts:
    - elapsed time in nanoseconds onto the :elapsed-time-ns key in data.
    - (an examlpe of) the expression value on the :expr-value key.
    - the number of evals on the :eval-count key."
  [{:keys [state] :as sample} measured]
  (let [f                         (:f measured)
        eval-count                (:eval-count measured)
        [elapsed-time expr-value] (f state eval-count)]
    (assoc sample
           :elapsed-time-ns elapsed-time
           :expr-value expr-value
           :eval-count eval-count)))


(defn with-class-loader-counts
  "Execute measured, adding class loading counts to the data map.

  Adds a to the :class-loader key in the sample data. The map contains
  the :loaded-count and :unloaded-count keys.

  Uses the ClassLoadingMXBean."
  [next-fn]
  (fn [sample measured]
    (assert (measured/measured? measured))
    (let [start (jvm/class-loader-counts)]
      (-> sample
         (next-fn measured)
         (assoc :class-loader (util/diff (jvm/class-loader-counts) start))))))


(defn with-compilation-time
  "Execute measured, add compilation time to the data map.

  Adds a map to the :compilation key in the sample data.  The map
  contains the :compilation-time key.

  Uses the CompilationMXBean."
  [next-fn]
  (fn [sample measured]
    (assert (measured/measured? measured))
    (let [start (jvm/compilation-time)]
      (-> sample
         (next-fn measured)
         (assoc :compilation (util/diff (jvm/compilation-time) start))))))


(defn with-memory
  "Execute measured, add compilation time to the data map.

  Adds a map to the :memory key in data.  The map contains sub-maps for
  each type of memory, and the total memory (on the :total key).  Each
  sub-map contains the :init, :committed, :max and :used keys.

  Uses the MemoryMXBean."
  [next-fn]
  (fn memory [sample measured]
    (assert (measured/measured? measured))
    (let [start (jvm/memory)]
      (-> sample
         (next-fn measured)
         (assoc :memory (util/diff (jvm/memory) start))))))


(defn with-runtime-memory
  "Execute measured, add runtime memory to the data map.

  Adds a map to the :runtime-memory key in sample data..

  Uses the java Runtime class."
  [next-fn]
  (fn [sample measured]
    (assert (measured/measured? measured))
    (let [start (jvm/runtime-memory)]
      (-> sample
         (next-fn measured)
         (assoc :runtime-memory (util/diff (jvm/runtime-memory) start))))))


(defn with-finalization-count
  "Execute measured, add pending finalization count to the data map.

  Adds maps to the :finalization key in data, with the :start, :finish,
  and :delta sub-keys.

  Uses the MemoryMXBean."
  [next-fn]
  (fn finalization-count [sample measured]
    (assert (measured/measured? measured))
    (let [start (jvm/finalization-count)]
      (-> sample
         (next-fn measured)
         (assoc :finalization (util/diff (jvm/finalization-count) start))))))


(defn with-garbage-collector-stats
  "Execute measured, add garbage collection counts and times to the data map.

  Uses the GarbageCollectorMXBean beans."
  [next-fn]
  (fn [data measured]
    (assert (measured/measured? measured))
    (let [start (jvm/garbage-collector-stats)]
      (-> data
         (next-fn measured)
         (assoc :garbage-collector
                (util/diff (jvm/garbage-collector-stats) start))))))


(def terminal-fns
  {:time-metric time-metric})

(def pipeline-fns
  {:class-loader       with-class-loader-counts
   :compilation-time   with-compilation-time
   :memory             with-memory
   :runtime-memory     with-runtime-memory
   :finalization-count with-finalization-count
   :garbage-collector  with-garbage-collector-stats})


(defn pipeline
  "Build a pipeline by specifying pipeline keywords.

  Returns a pipeline."
  [pipeline-kws & [{:keys [terminal-fn]
                    :or   {terminal-fn time-metric}}]]
  (reduce
    (fn [pipeline pipeline-kw]
      (let [f (pipeline-fns pipeline-kw)]
        (f pipeline)))
    terminal-fn
    pipeline-kws))


(defn execute
  "Executes a measured pipeline.

  e.g, to collect execution time, and result value for executing
  some-expr, use:

     (toolkit/execute
       toolkit/with-time
       (toolkit/measured-expr some-expr))"
  [pipeline measured]
  {:pre [(measured/measured? measured)]}
  (let [state ((:state-fn measured))
        sample {:state state}]
    (pipeline sample measured)))


(defn sample
  "Sample by invoking measured using pipeline.

  Invokes until time-budget-ns elapsed or eval-budget reached.

  Collects a sample data map for each invocation.  Map keys are
  determined by the pipeline.

  Return a sequence of sample maps."
  [pipeline
   measured
   {:keys [time-budget-ns
           eval-budget]
    :or   {time-budget-ns 500000000
           eval-budget    1000000}
    :as   _options}]
  (let [eval-count (long (:eval-count measured))]
    (loop [samples        []
           eval-budget    (long eval-budget)
           time-budget-ns (long time-budget-ns)]
      (if (and (pos? eval-budget) (pos? time-budget-ns))
        (let [sample (execute pipeline measured)
              t      (long (:time sample))]
          (recur
            (conj samples sample)
            (unchecked-subtract eval-budget eval-count)
            (unchecked-subtract time-budget-ns t)))
        samples))))


(defn total
  "Sum measured values across samples."
  [samples]
  (reduce util/sum samples))

(defn divide
  "Divide values across samples."
  [sample divisor]
  (walk/postwalk
    #(cond
       (integer? %) (quot % divisor)
       (number? %) (/ % divisor)
       :else %)
    sample))


;;; Sample metric accessors

(defn elapsed-time
  ^long [sample]
  (:elapsed-time-ns sample))

(defn total-memory
  ^long [sample]
  (-> sample :memory :total :used))
