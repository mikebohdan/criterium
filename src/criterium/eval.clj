(ns criterium.eval
  "Expression evaluation")


;;; Mutable place to avoid JIT removing expr evaluation altogether

(defprotocol MutablePlace
  "Provides functions to get and set a mutable place"
  (set-place [_ v] "Set mutable field to value.")
  (get-place [_] "Get mutable field value."))

(deftype Unsynchronized
    [^{:unsynchronized-mutable true :tag Object} v]
  MutablePlace
  (set-place [_ value] (set! v value))
  (get-place [_] v))

(def mutable-place
  "An object with a mutable, unsychronized field.

  Used to store the result of each expression, to prevent JIT
  optimizing away the expression entirely.

  The field is accessed with the MutablePlace protocol."
  (Unsynchronized. nil))

(defn sink-value
  "Sink the value into a mutable place."
  [v]
  (set-place mutable-place v))

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
