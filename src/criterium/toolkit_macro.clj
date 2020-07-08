(ns criterium.toolkit-macro)

;;; Instrumentation

(defn assoc-delta
  "Assoc finish merged with start using op, onto the :delta key."
  [{:keys [start finish] :as data}]
  (assoc data :delta (util/diff finish start)))


;; Macros to wrap an expr execution.  The macros use a first data
;; argument to accumulate data about the expr evaluation.


(defmacro instrumented
  "Introduces an instrumented expression expr.

  The expr contains nested with-* macro calls, and returns
  a map with the collected data.  The inner-most with- form
  must be either with-expr, or with-expr-value.

  e.g, to collect execution time, and result value for executing
  some-expr, use:

     (toolkit/instrumented
       (toolkit/with-time
         (toolkit/with-expr-value
           some-expr)))
  "
  [expr]
  `(-> {} ~expr))


(defmacro with-expr-value
  "Execute expr, adding the return value to the data map's :expr-value key."
  [data expr]
  `(assoc ~data :expr-value ~expr :num-evals 1))


(defmacro with-expr
  "Execute expr."
  [data expr]
  `(let [d# ~data]
     ~expr
     (assoc d# :num-evals 1)))


(defmacro with-time
  "Execute expr, adding timing to the data map.

  Adds maps to the :time key in data, with the :before, :after, and :delta sub-data.
  Each map contains the :elapsed key with a timestamp in nanoseconds."
  [data expr]
  `(-> ~data
       (assoc-in [:time :start] {:elapsed (jvm/timestamp)})
       ~expr
       (assoc-in [:time :finish] {:elapsed (jvm/timestamp)})
       (update-in [:time] assoc-delta)))


(defmacro with-class-loader-counts
  "Execute expr, adding class loading counts to the data map.

  Adds maps to the :class-loader key in data, with the :before, :after, and :delta
  sub-keys.  Each map contains the :loaded-count and :unloaded-count keys.

  Uses the ClassLoadingMXBean."
  [data expr]
  `(-> ~data
       (assoc-in [:class-loader :start] (jvm/class-loader-counts))
       ~expr
       (assoc-in [:class-loader :finish] (jvm/class-loader-counts))
       (update-in [:class-loader] assoc-delta)))


(defmacro with-compilation-time
  "Execute expr, add compilation time to the data map.

  Adds maps to the :compilation key in data, with the :start, :finish, and :delta
  sub-keys.  Each map contains the :compilation-time key.

  Uses the CompilationMXBean."
  [data expr]
  `(-> ~data
       (assoc-in [:compilation :start] (jvm/compilation-time))
       ~expr
       (assoc-in [:compilation :finish] (jvm/compilation-time))
       (update-in [:compilation] assoc-delta)))


(defmacro with-memory
  "Execute expr, add compilation time to the data map.

  Adds maps to the :memory key in data, with the :start, :finish, and :delta sub-keys.
  Each map contains sub-maps for each type of memory, and the total memory (on
  the :total key).  Each sub-map contains the :init, :committed, :max and :used keys.

  Uses the MemoryMXBean."
  [data expr]
  `(-> ~data
       (assoc-in [:memory :start] (jvm/memory))
       ~expr
       (assoc-in [:memory :finish] (jvm/memory))
       (update-in [:memory] assoc-delta)))


(defmacro with-runtime-memory
  "Execute expr, add runtime memory to the data map.

  Adds maps to the :runtime-memory key in data, with the :start, :finish,
  and :delta sub-keys.

  Uses the java Runtime class."
  [data expr]
  `(-> ~data
       (assoc-in [:runtime-memory :start] (jvm/runtime-memory))
       ~expr
       (assoc-in [:runtime-memory :finish] (jvm/runtime-memory))
       (update-in [:runtime-memory] assoc-delta)))


(defmacro with-finalization-count
  "Execute expr, add pending finalization count to the data map.

  Adds maps to the :finalization key in data, with the :start, :finish,
  and :delta sub-keys.

  Uses the MemoryMXBean."
  [data expr]
  `(-> ~data
       (assoc-in [:finalization :start] (jvm/finalization-count))
       ~expr
       (assoc-in [:finalization :finish] (jvm/finalization-count))
       (update-in [:finalization] assoc-delta)))


(defmacro with-garbage-collector-stats
  "Execute expr, add garbage collection counts and times to the data map.

  Uses the GarbageCollectorMXBean beans."
  [data expr]
  `(-> ~data
       (assoc-in [:garbage-collector :start] (jvm/garbage-collector-stats))
       ~expr
       (assoc-in [:garbage-collector :finish] (jvm/garbage-collector-stats))
       (update-in [:garbage-collector] assoc-delta)))


(defn deltas
  "Return a data map containing only the delta data.

  Discards all :start and :finish values, and moves :delta
  values up a level in the map."
  [data]
  (reduce-kv
    (fn [data k v]
      (if-let [delta (and (map? v) (:delta v))]
        (assoc data k delta)
        (assoc data k v)))
    {}
    data))

;;; Memory management

(defn force-gc
  "Force garbage collection and finalisers so that execution time
  associated with this is not incurred at another time. Up to
  max-attempts are run to clear all pending finalizers and free as
  much memory as possible.

  Returns the GC execution time,  total changes in memory, and in
  object finalizers pending."
  [max-attempts]
  (loop [all-deltas [] ; hold onto data we allocate here
         attempts   0]
    (let [deltas (deltas
                   (instrumented
                     (with-memory
                       (with-finalization-count
                         (with-time
                           (with-expr
                             (jvm/run-finalization-and-force-gc)))))))]

      (let [new-memory-used (-> deltas :memory :total :used)]
        (if (and (< attempts max-attempts)
                 (or (pos? (-> deltas :finalization :pending))
                     (< new-memory-used 0)))
          (recur (conj all-deltas deltas)
                 (inc attempts))
          (reduce util/sum all-deltas))))))

;;; timing

(defn estimate-execution-time
  "Return an initial estimate for the execution time of a measured function.

  Repeatedly times the invocation of the function and returns the minimum invocation
  time.

  For quick functions limit execution count, while for slower functions limit total
  execution time. Limit evaluations to eval-budget, or elapsed time to time-budget-ns."
  [{:keys [state-fn f]}
   {:keys [time-budget-ns eval-budget]
    :or {time-budget-ns 1000000000 eval-budget 1000}}]
  (let [measure-time (fn [] (let [state (state-fn)]
                             (-> (instrumented
                                  (with-time
                                    (with-expr-value
                                      (f state))))
                                :time :elapsed)))]
    (loop [t              (measure-time)
           estimated-t    t
           time-budget-ns (- time-budget-ns t)
           eval-budget    (dec eval-budget)]
      (if (and (pos? time-budget-ns)
               (pos? eval-budget))
        (recur (measure-time)
               (min t estimated-t)
               (- time-budget-ns estimated-t)
               (dec eval-budget))
        (min t estimated-t)))))
