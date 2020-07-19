(ns criterium.measured
  "Defines the Measured type and measure-batch"
  (:require [criterium
             [eval :as eval]]))

;; Measured type
(deftype Measured
    [^clojure.lang.IFn state-fn
     ^clojure.lang.IFn f
     ^long eval-count])

(defn measured?
  "Predicate for x being a Measured"
  [x]
  (instance? Measured x))


(defn measured
  "Return a Measured for a function that can be benchmarked.

  The basic unit of measurement, a Measured consists of a state
  generation funtion and a function to be measured.  The function to be
  measured takes the result of calling the state function as an
  argument, ie. (f (state-fn)).

  The call of the state function is not included in the benchmark
  timings, etc.

  The state function is used to prevent constant folding for constant
  inputs.

  The eval-count allows usage where the function is a wrapper that
  evaluates the subject function multiple times.
  "
  ^Measured
  [^clojure.lang.IFn state-fn
   ^clojure.lang.IFn f
   ^long eval-count]
  (Measured. state-fn f eval-count))


(defn- measured-expr*
  "Return a measured function for the given expression.

  The arguments are converted into a vector, which is used as an
  argument to the a function that wraps the expression.

  Any expr that is not a List is treated as a constant.  This is mainly
  for internal benchmarking."
  [expr]
  (if (list? expr)
    (let [args     (vec (drop 1 expr))
          arg-syms (vec (repeatedly (count args) (fn [] (gensym "arg"))))]
      `(measured
         (fn [] ~args)
         (fn [~arg-syms] (~(first expr) ~@arg-syms))
         1))
    `(measured
       (fn [] ~expr)
       identity
       1)))


(defmacro expr
  "Return a Measured for the given expression.

  The expression is wrapped in a function.

  When the expression is a list form, it is treated as a function call.
  The function arguments are treated as constant expressions and are
  hoisted into a state function.  The result of the state function is a
  vector that is passed to the function wrapper as a vector and
  destructured."
  [expr]
  (measured-expr* expr))

(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing true)

(defn batch
  "Wrap `measured` to run `eval-count` times.

  Inside the loop, the return value of each execution is put into a sink,
  The sink function can be customised by passing the `:sink-fn` options.

  Return a Measured."
  [^Measured m
   eval-count
   & [{:keys [^clojure.lang.IFn sink-fn]
       :or {sink-fn eval/sink-object}}]]
  {:pre [(>= eval-count 1)]}
  (let [eval-count (long eval-count)
        ^clojure.lang.IFn f (.f m)]
    (measured
      (.state-fn ^Measured m)
      (fn [state]
        (loop [i eval-count]
          (when (pos? i)
            (sink-fn (f state)) ;
            (recur (unchecked-dec i)))))
      (* eval-count (.eval-count m)))))

(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing false)

(defn invoke
  "Invoke the given Measured.

  Calls the Measured's function with the result of calling the
  Measured's state function."
  [^Measured measured]
  ((.f measured) ((.state-fn measured))))
