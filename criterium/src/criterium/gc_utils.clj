(ns criterium.gc-utils
  "Provide a mechanism to (briefly) disable GC around a block of code."
  (:import
   [java.lang.ref
    Reference]
   [java.util.concurrent
    Executors
    Semaphore]))


(def reference-lock
  "The lock we want to synchronise on."
  (let [field (.getDeclaredField Reference "lock")]
    (.setAccessible field true)
    (.get field nil)))

(def executor-service (Executors/newSingleThreadExecutor))

(def held-semaphore (Semaphore. 0))
(def finished-semaphore (Semaphore. 0))
(defn monitor-holder []
  (locking reference-lock
    (.release held-semaphore)
    (.acquireUninterruptibly finished-semaphore)))

(defn with-no-gc*
  "Run f with no gc"
  [f]
  (let [holder (.submit executor-service monitor-holder)]
    (try
      (.acquireUninterruptibly held-semaphore)
      (when-not (Thread/interrupted)
        (f))
      (finally
        (when-not (Thread/interrupted)
          (.release finished-semaphore))
        @holder))))

(defmacro with-no-gc
  "Run body, preventing the initiation of a GC.
  A GC may still be in progress on entry to this block."
  [& body]
  `(with-no-gc* (fn [] ~@body)))

;; (with-no-gc []) ; should work with no problems
;; (with-no-gc (.gc (Runtime/getRuntime))) ; should block the jvm
