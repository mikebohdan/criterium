(ns criterium.pipeline
  "A measured pipeline.

  A pipeline is a function that takes a sample state and a measured
  and returns an updated sample state.  It is usually called via the
  execute function.

  A pipeline can be composed via pipeline functions and a pipeline
  terminal function."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [criterium.domain :as domain]
   [criterium.gc-utils :as gc-utils]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]
   [criterium.util :as util]))

;; Metrics

;; Pipeline functions to wrap a Neasured execution.

(defn elapsed-time-metric
  "A terminal function to execute measured, adding results to the data map.

  Puts:
    - elapsed time in nanoseconds onto the :elapsed-time-ns key in data.
    - (an examlpe of) the expression value on the :expr-value key.
    - the number of evals on the :eval-count key."
  [{:keys [eval-count state] :as sample} measured]
  (let [f                         (:f measured)
        [elapsed-time expr-value] (f state eval-count)]
    (assoc sample
           :elapsed-time-ns elapsed-time
           :expr-value expr-value)))

(defn with-cpu-time
  "Execute measured, adding cpu execution time in ns to the data map.

  Adds the :cpu-time-ns key in the sample data.

  Uses the ThreadMXBean."
  [next-fn]
  (fn cpu-time [sample measured]
    (assert (measured/measured? measured))
    (let [start  (jvm/current-thread-cpu-time)
          sample (next-fn sample measured)
          finish (jvm/current-thread-cpu-time)]
      (assoc sample :cpu-time-ns (unchecked-subtract finish start)))))

(defn with-class-loader-counts
  "Execute measured, adding class loading counts to the data map.

  Adds a to the :class-loader key in the sample data. The map contains
  the :loaded-count and :unloaded-count keys.

  Uses the ClassLoadingMXBean."
  [next-fn]
  (fn class-loader-counts [sample measured]
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
  (fn compilation-time [sample measured]
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
    (let [start  (jvm/memory)
          sample (next-fn sample measured)
          finish (jvm/memory)]
      (assoc sample :memory (util/diff finish start)))))

(defn with-no-gc
  "Execute measured, preventing initiation of GC.
  Experimental - may not work after Java 9.
  Using this can result in the JVM being completely blocked."
  [next-fn]
  (fn no-gc [sample measured]
    (assert (measured/measured? measured))
    (gc-utils/with-no-gc
      (next-fn sample measured))))

(defn with-force-gc
  "Execute measured, forcing a GC attempt."
  [next-fn]
  (fn force-gc [sample measured]
    (assert (measured/measured? measured))
    (jvm/force-gc)
    (next-fn sample measured)))

(defn with-runtime-memory
  "Execute measured, add runtime memory to the data map.

  Adds a map to the :runtime-memory key in sample data..

  Uses the java Runtime class."
  [next-fn]
  (fn runtime-memory [sample measured]
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
  (fn garbage-collector-stats [data measured]
    (assert (measured/measured? measured))
    (let [start (jvm/garbage-collector-stats)]
      (-> data
          (next-fn measured)
          (assoc :garbage-collector
                 (util/diff (jvm/garbage-collector-stats) start))))))

(def terminal-fns
  {:elapsed-time-ns elapsed-time-metric})

(defn terminal-fn?
  [fn-kw]
  ((set (keys terminal-fns)) fn-kw))

(def pipeline-fns
  {:cpu-time-ns        with-cpu-time
   :class-loader       with-class-loader-counts
   :compilation-time   with-compilation-time
   :memory             with-memory
   :force-gc           with-force-gc
   :no-gc              with-no-gc
   :runtime-memory     with-runtime-memory
   :finalization-count with-finalization-count
   :garbage-collector  with-garbage-collector-stats})

(s/def ::pipeline-fn-kw (set (keys pipeline-fns)))

(defn pipeline
  "Build a pipeline by specifying pipeline function keywords.

  Returns a pipeline."
  [{:keys [stages terminator] :as pipeline-config}]
  (let [terminal-fn (terminal-fns terminator)]
    (when-not terminal-fn
      (throw (ex-info "Unknown terminator function"
                      {:stages     stages
                       :terminator terminator})))
    (with-meta
      (reduce
       (fn [pipeline stage]
         (let [f (pipeline-fns stage)]
           (when-not f
             (throw (ex-info "Unknown pipeline function" {:stage stage})))
           (f pipeline)))
       terminal-fn
       stages)
      {:config pipeline-config})))

(defn metrics
  "Return a sequence of all metrics produced by a pipeline with the
  given spec."
  [{:keys [stages terminator]}]
  (conj stages terminator))

(defrecord Sample
    ;; record to ensure no allocation by the time-metric terminator
    [^long elapsed-time-ns
     ^long eval-count
     expr-value
     state])

(defn execute
  "Executes a measured pipeline.

  e.g, to collect execution time, and result value for executing
  some-expr, use:

     (toolkit/execute
       toolkit/with-time
       (toolkit/measured-expr some-expr))"
  [pipeline measured eval-count]
  {:pre [(measured/measured? measured)]}
  (let [state  ((:state-fn measured))
        sample (map->Sample
                {:elapsed-time-ns 0
                 :eval-count      eval-count
                 :state           state})]
    (pipeline sample measured)))


;; (defn sample
;;   "Sample by invoking measured with eval-count using pipeline.

;;   Invokes until time-budget-ns elapsed or eval-budget reached.

;;   Collects a sample data map for each invocation.  Map keys are
;;   determined by the pipeline.

;;   Return a sequence of sample maps."
;;   [pipeline
;;    measured
;;    eval-count
;;    {:keys [time-budget-ns
;;            eval-budget]
;;     :or   {time-budget-ns 500000000
;;            eval-budget    1000000}
;;     :as   _options}]
;;   (let [eval-count (long eval-count)]
;;     (loop [samples        []
;;            eval-budget    (long eval-budget)
;;            time-budget-ns (long time-budget-ns)]
;;       (if (and (pos? eval-budget) (pos? time-budget-ns))
;;         (let [sample (execute pipeline measured eval-count)
;;               _ (println "sample" sample)
;;               t      (long (:elapsed-time-ns sample))]
;;           (recur
;;             (conj samples sample)
;;             (unchecked-subtract eval-budget eval-count)
;;             (unchecked-subtract time-budget-ns t)))
;;         samples))))


(defn total
  "Sum measured values across samples."
  [samples]
  (reduce util/sum samples))

(defn divide
  "Divide values across samples."
  [sample divisor]
  {:pre [(some? divisor) (number? divisor) (not= 0 divisor)]}
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

(defn heap-memory
  ^long [sample]
  (-> sample :memory :heap :used))

(defn compilation-time
  ^long [sample]
  (-> sample :compilation :compilation-time))

(defn gc-counts
  ^long [sample]
  (-> sample :garbage-collector :total :count))

;;; Spec definitions

(s/def ::state any?)
(s/def ::expr-value any?)
(s/def ::state any?)

(s/def ::sample (s/keys :req-un [::state ::domain/eval-count]))

(s/def ::pipeline-fn
  (s/fspec
   :args (s/cat :sample (s/keys :req-un [::state ::domain/eval-count])
                :measured ::measured/measured)
   :ret  (s/keys
          :req-un [::state ::domain/eval-count
                   ::domain/elapsed-time-ns
                   ::expr-value ::domain/eval-count])))

(s/fdef time-metric
  :args (s/cat :sample (s/keys :req-un [::state ::domain/eval-count])
               :measured ::measured/measured)
  :ret  (s/keys
         :req-un [::state ::domain/eval-count
                  ::domain/elapsed-time-ns
                  ::expr-value ::domain/eval-count]))

(s/fdef with-class-loader-counts
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-compilation-time
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-memory
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-runtime-memory
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-finalization-count
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/fdef with-garbage-collector-stats
  :args (s/cat :fn ::pipeline-fn)
  :ret  ::pipeline-fn)

(s/def ::terminal-fn-kw keyword?)

(s/fdef elapsed-time
  :args (s/cat :sample ::sample)
  :ret ::domain/elapsed-time-ns
  :fn #(= (:ret %) (-> % :args :sample :elapsed-time-ns)))

(s/def ::total-memory (s/and number? pos?))

(s/fdef total-memory
  :args (s/cat :sample ::sample)
  :ret ::total-memory
  :fn #(= (:ret %) (-> % :args :sample :memory :total :used)))

(s/fdef divide
  :args (s/cat :sample ::sample :divisor number?)
  :ret ::sample
  :fn #(= (-> % :ret :elapsed-time-ns)
          (/ (-> % :args :sample :elapsed-time-ns)
             (-> % :args :divisor))))

(s/def ::pipeline-fn-kws (s/coll-of ::pipeline-fn-kw))
(s/def ::stages ::pipeline-fn-kws)
(s/def ::termonator ::terminal-fn-kw)
(s/def ::pipeline-config (s/keys :req-un [::stages ::terminator]))

(s/fdef pipeline
  :args ::pipeline-config
  :ret ::pipeline-fn)

(s/fdef execute
  :args (s/cat :pipeline ::pipeline-fn
               :measured ::measured/measured
               :eval-count ::domain/eval-count)
  :ret ::sample)
