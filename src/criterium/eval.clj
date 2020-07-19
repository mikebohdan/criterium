(ns criterium.eval
  "Expression evaluation"
  (:require [criterium
             [jvm :as jvm]])
  (:import [java.util Random]
           [java.lang.ref WeakReference]))


;;; Mutable place to avoid JIT removing expr evaluation altogether

(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing true)

;; We use deftype and definterface to minimise runtime costs
(definterface SinkObject
  (^void sinkObject [^Object x]))

(deftype ObjectSink
    [^{:volatile-mutable true :tag long} m
     ^{:unsynchronized-mutable true :tag long} r
     ^{:unsynchronized-mutable true} obj]
  SinkObject
  (^void sinkObject [_ ^Object sv]
   (let [mm m]
     (set! r (* r 0x19660D 0x3C6EF35F))
     (when (zero? (bit-and r mm))
       (set! obj (WeakReference. sv))
       (set! m (unchecked-inc (bit-shift-left mm 1)))))))

(let [^ObjectSink object-sink (ObjectSink.
                                1
                                (.nextLong (Random. (jvm/timestamp)))
                                nil)]
  ;; An object which can sink objects.

  ;; Used to store the result of each expression, to prevent JIT
  ;; optimizing away the expression entirely.

  (defn sink-object
    "Sink the value into a mutable place,
  Only writes if the it fails an identity check with a volatile read
  of the mutable place, which should never happen."
    [v]
    (.sinkObject object-sink v)))

(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing false)

(definterface SinkPrimitiveLong
  (^void sinkPrimitiveLong [^long x]))

(deftype PrimitiveLongSink
    [^{:volatile-mutable true :tag 'long} v]
  SinkPrimitiveLong
  (^void sinkPrimitiveLong [_ ^long sv]
   (when (= v sv)
     (set! v sv))))

(let [^PrimitiveLongSink primitive-log-sink
      (PrimitiveLongSink. (long Long/MAX_VALUE))]
  ;; An object which can sink primitive longs.

  ;; Used to store the result of each expression, to prevent JIT
  ;; optimizing away the expression entirely.

  (defn sink-primitive-long
    "Sink the primitive long value into a mutable place,
  Only writes if the it fails an identity check with a volatile read
  of the mutable place, which should never happen."
    [^long v]
    (.sinkPrimitiveLong primitive-log-sink v)))

;;; Execution

(defmacro execute-n-times-unrolled
  "Evaluates `expr` `n` times, each time saving the
  return value as an Object in `mutable-place`.

  Except for the expr evaluation, storage of the expression
  value is the only overhead.

  The JVM is not free to optimize away the evaluations of expr, as the
  values are saved in `mutable-place`."
  [expr n]
  `(do
     ~@(for [i (range n)]
         `(set-place mutable-place ~expr))
     (get-place mutable-place)))  ; use the mutable value for good measure

(defmacro execute-n-times
  "Evaluates `expr` `n` times, each time saving the
  return value as an Object in `mutable-place`.

  Except for the expr evaluation, only a few primitive long arithmetic
  operations and comparisons to 0, and the storage of the return
  value, are done during each iteration.

  The JVM is not free to optimize away the evaluations of expr, as the
  values are saved in `mutable-place`."
  [expr n]
  `(do
     (loop [i# (long (dec ~n))
            v# ~expr]
       (set-place mutable-place v#)
       (if (pos? i#)
         (recur (unchecked-dec i#) ~expr)
         v#))
     (get-place mutable-place)))  ; use the mutable value for good measure

(defmacro execute-fn-n-times
  "Evaluates `(f)` `n` times, each time saving the
  return value as an Object in `mutable-place`.

  Except for the call to (f), only a few primitive long arithmetic
  operations and comparisons to 0, and the storage of the return
  value, are done during each iteration.

  The JVM is not free to optimize away the calls to f, as the return
  values are saved in `mutable-place`."
  [f n]
  `(loop [i# (long (dec ~n))
          v# (~f)]
     (set-place mutable-place v#)
     (if (pos? i#)
       (recur (unchecked-dec i#) (~f))
       v#)))
