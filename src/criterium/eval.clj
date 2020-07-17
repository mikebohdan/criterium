(ns criterium.eval
  "Expression evaluation")


;;; Mutable place to avoid JIT removing expr evaluation altogether

;; We use deftype and definterface to minimise runtime costs
(definterface SinkObject
  (^void sinkObject [^Object x]))

(deftype ObjectSink
    [^{:volatile-mutable true :tag Object} v]
  SinkObject
  (^void sinkObject [_ ^Object sv]
   (when (identical? v sv)
     (set! v sv))))

(def ^ObjectSink object-sink
  "An object which can sink objects.

  Used to store the result of each expression, to prevent JIT
  optimizing away the expression entirely."
  (ObjectSink. ::unique-val))

(defn sink-object
  "Sink the value into a mutable place,
  Only writes if the it fails an identity check with a volatile read
  of the mutable place, which should never happen."
  [v]
  (.sinkObject object-sink v))


(definterface SinkPrimitiveLong
  (^void sinkPrimitiveLong [^long x]))

(deftype PrimitiveLongSink
    [^{:volatile-mutable true :tag 'long} v]
  SinkPrimitiveLong
  (^void sinkPrimitiveLong [_ ^long sv]
   (when (= v sv)
     (set! v sv))))

(def ^PrimitiveLongSink primitive-log-sink
  "An object which can sink primitive longs.

  Used to store the result of each expression, to prevent JIT
  optimizing away the expression entirely."
  (PrimitiveLongSink. (long Long/MAX_VALUE)))

(defn sink-primitive-long
  "Sink the primitive long value into a mutable place,
  Only writes if the it fails an identity check with a volatile read
  of the mutable place, which should never happen."
  [^long v]
  (.sinkPrimitiveLong primitive-log-sink v))

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
