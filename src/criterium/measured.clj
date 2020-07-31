(ns criterium.measured
  "Defines the Measured type and measure-batch"
  (:require [criterium
             [eval :as eval]])
  (:import [criterium.measured Helpers]
           [org.openjdk.jmh.infra Blackhole]))

;; Measured type
(defrecord Measured
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
  argument, ie. `(f (state-fn))`, and returns an
  `[elapsed-time expr-value]` tuple.

  The state function is used to prevent constant folding for constant
  inputs.

  The eval-count allows usage where the function is a wrapper that
  evaluates the subject expression multiple times.
  "
  ^Measured
  [^clojure.lang.IFn state-fn
   ^clojure.lang.IFn f
   ^long eval-count]
  (->Measured state-fn f eval-count))


(defn ^:internal measured-expr-fn
  [arg-syms expr & [{:keys [arg-metas]}]]
  (println "arg-metas" arg-metas)
  (let [blackhole-sym  (with-meta (gensym "blachole")
                         {:tag 'org.openjdk.jmh.infra.Blackhole})
        eval-count-sym (gensym "eval-count")
        ;; eval-count-sym (with-meta (gensym "eval-count")
        ;;                  {:tag 'long})
        ;;     sym-meta (clojure.walk/postwalk
        ;;                (fn [f]
        ;;                  (println :f f (type f))
        ;;                  (cond
        ;;                    (symbol? f) (do
        ;;                                  (println :symbol)
        ;;                                  [f])
        ;;                    (sequential? f) (do
        ;;                                      (println :sequential
        ;;                                               (filterv vector? f)
        ;;                                               (vec(mapcat
        ;;                                                     identity
        ;;                                                     (filterv vector? f))))
        ;;                                      (vec (mapcat identity
        ;;                                                  (filterv vector? f))))
        ;;                    :else (do
        ;;                            (println :other)
        ;;                            nil)))
        ;;                expr)
        ;;     sym-meta (zipmap sym-meta sym-meta)
        ;;     arg-syms (mapv #(sym-meta % %) arg-syms)
        ]
    ;; (println "measured-expr-fn sym-mets" sym-meta (mapv meta sym-meta))
    ;; (println "measured-expr-fn arg-syms" arg-syms (pr-str (mapv meta arg-syms)))
    `(;; do ; let [f# (fn [~@arg-syms] ~expr)]
      fn [~arg-syms ~eval-count-sym]
      (let [~blackhole-sym eval/blackhole ; hoist car lookup out of loop
            ~@(mapcat
                (fn [arg-sym arg-meta]
                  (println "hinting" arg-sym arg-meta)
                  (let [tag (:tag arg-meta)]
                    (if (and (symbol? tag) (#{'long 'int} tag))
                      [arg-sym
                       (list tag arg-sym)]
                      [(with-meta arg-sym arg-meta)
                       arg-sym]
                      )))
                arg-syms arg-metas)

            n#             (unchecked-dec (long ~eval-count-sym))
            ;; elapsed#       (Helpers/loop ~blackhole-sym n# f#
            ;; ~@arg-syms)
            start#         (criterium.jvm/timestamp)
            val#           ~expr
            ]
        (loop [i# n#]
          (when (pos? i#)
            (.consume ~blackhole-sym ~expr)
            (recur (unchecked-dec i#))))
        (let [finish# (criterium.jvm/timestamp)]
          (eval/evaporate)
          [(unchecked-subtract finish# start#) nil])))))


(comment
  (measured-expr-fn
    ['a 'b]
    `(f ~@['a'b])
    {:arg-metas [{:tag 'Long}{:tag 'Long}]}))

(defn- measured-expr*
  "Return a measured function for the given expression.

  The arguments are converted into a vector, which is used as an
  argument to the a function that wraps the expression.

  Any expr that is not a List is treated as a constant.  This is mainly
  for internal benchmarking."
  [expr & [options]]
  (println "measured-expr* options" options)
  (println "list?" (list? expr) (type expr))
  (if (or (list? expr) (instance? clojure.lang.Cons expr))
    (let [args     (vec (drop 1 expr))
          arg-syms (vec (repeatedly (count args) (fn [] (gensym "arg"))))]
      `(measured
         (fn [] ~args)
         ~(measured-expr-fn arg-syms `(~(first expr) ~@arg-syms) options)
         ;; (fn [~arg-syms eval-count#]
         ;;   (let [~blackhole-sym eval/blackhole
         ;;         n#             (dec eval-count#)
         ;;         start#         (jvm/timestamp)
         ;;         expr-value#    (~(first expr) ~@arg-syms)]
         ;;     (loop [i# n#]
         ;;       (when (pos? i#)
         ;;         ;;(.sink sink (f state))
         ;;         (.consume ~blackhole-sym (~(first expr) ~@arg-syms))
         ;;         (recur (unchecked-dec i#))))
         ;;     (let [finish# (jvm/timestamp)]
         ;;       (eval/evaporate)
         ;;       [(unchecked-subtract finish# start#) expr-value#])))
         1))
    (let [arg-sym (gensym "expr-val")]
      `(measured
         (fn [] [~expr])
         ~(measured-expr-fn [arg-sym] arg-sym options)
         ;; (fn [expr-value# eval-count#]
         ;;   (let [~blackhole-sym eval/blackhole
         ;;         n#             (dec (long eval-count#))
         ;;         start#         (jvm/timestamp)
         ;;         expr-value#    expr-value#]
         ;;     (loop [i# n#]
         ;;       (when (pos? i#)
         ;;         ;;(.sink sink (f state))
         ;;         (.consume ~blackhole-sym expr-value#)
         ;;         (recur (unchecked-dec i#))))
         ;;     (let [finish# (jvm/timestamp)]
         ;;       (eval/evaporate)
         ;;       [(unchecked-subtract finish# start#) expr-value#])))
         1))
    ))

(comment
  (measured-expr*
    `(nth v 1)
    {:arg-metas [{:tag 'Long}{:tag 'Long}]}))

;; (let [blackhole-sym (with-meta (gensym "blachole")
;;                         {:tag org.openjdk.jmh.infra.Blackhole})]
;;     (if (list? expr)
;;       (let [args     (vec (drop 1 expr))
;;             arg-syms (vec (repeatedly (count args) (fn [] (gensym "arg"))))]
;;         `(measured
;;            (fn [] ~args)
;;            (fn [~arg-syms eval-count#]
;;              (let [~blackhole-sym eval/blackhole
;;                    n#             (dec eval-count#)
;;                    start#         (jvm/timestamp)
;;                    expr-value#    (~(first expr) ~@arg-syms)]
;;                (loop [i# n#]
;;                  (when (pos? i#)
;;                    ;;(.sink sink (f state))
;;                    (.consume ~blackhole-sym (~(first expr) ~@arg-syms))
;;                    (recur (unchecked-dec i#))))
;;                (let [finish# (jvm/timestamp)]
;;                  (eval/evaporate)
;;                  [(unchecked-subtract finish# start#) expr-value#])))
;;            1))
;;       `(measured
;;          (fn [] ~expr)
;;          (fn [expr-value# eval-count#]
;;            (let [~blackhole-sym eval/blackhole
;;                  n#             (dec (long eval-count#))
;;                  start#         (jvm/timestamp)
;;                  expr-value#    expr-value#]
;;              (loop [i# n#]
;;                (when (pos? i#)
;;                  ;;(.sink sink (f state))
;;                  (.consume ~blackhole-sym expr-value#)
;;                  (recur (unchecked-dec i#))))
;;              (let [finish# (jvm/timestamp)]
;;                (eval/evaporate)
;;                [(unchecked-subtract finish# start#) expr-value#])))
;;          1)
;;       ;; `(measured
;;       ;;    (fn [] ~expr)
;;       ;;    (fn [x]
;;       ;;      (let [start#  (jvm/timestamp)
;;       ;;            finish# (jvm/timestamp)]
;;       ;;        [(unchecked-subtract finish# start#) x]))
;;       ;;    1)
;;       ))

(defmacro expr
  "Return a Measured for the given expression.

  The expression is wrapped in a function.

  When the expression is a list form, it is treated as a function call.
  The function arguments are treated as constant expressions and are
  hoisted into a state function.  The result of the state function is a
  vector that is passed to the function wrapper as a vector and
  destructured."
  [expr & [options]]
  (measured-expr* expr options))

(comment
  (def v 1)
  (expr
    (nth v 1)
    {:arg-metas [{:tag 'Long}{:tag 'Long}]})
  (expr (nth v 1)))

(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing true)

(defn batch
  "Wrap `measured` to run `eval-count` times.

  Inside the loop, the return value of each execution is put into a sink,
  The sink function can be customised by passing the `:sink-fn` options.

  Return a Measured."
  [m
   eval-count
   ;; & [;; {:keys [^clojure.lang.IFn sink-fn]
   ;;     ;; :or {sink-fn eval/sink-object}
   ;;     }]
  ]
  {:pre [(>= eval-count 1)]}
  (let [eval-count (long eval-count)
        ;; ^clojure.lang.IFn f (.f m)
        ;; ^criterium.eval.Sink sink eval/sink  ; hoist var lookup out of loop
        ;; ^org.openjdk.jmh.infra.Blackhole blackhole eval/blackhole
        ]
    (assoc m :eval-count eval-count)
    ;; (measured
    ;;   (:state-fn m)
    ;;   (fn [state eval-count]
    ;;     ;;(Measured/batch blackhole f state eval-count)
    ;;     (let [^org.openjdk.jmh.infra.Blackhole blackhole eval/blackhole
    ;;           ^clojure.lang.IFn f (:f m)
    ;;           n (dec eval-count)
    ;;           start (jvm/timestamp)
    ;;           expr-value# (f state)]
    ;;       (loop [i n]
    ;;         (when (pos? i)
    ;;           ;;(.sink sink (f state))
    ;;           (.consume blackhole (f state))
    ;;           (recur (unchecked-dec i))))
    ;;       (let [finish (jvm/timestamp)]
    ;;         [(unchecked-subtract finish start) nil])))
    ;;   eval-count)
    ))

(alter-var-root #'*compiler-options*
                assoc :disable-locals-clearing false)

(defn invoke
  "Invoke the given Measured.

  Calls the Measured's function with the result of calling the
  Measured's state function."
  ([^Measured measured]
   (invoke measured 1))
  ([^Measured measured eval-count]
   ((.f measured) ((:state-fn measured)) eval-count)))
