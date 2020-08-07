(ns criterium.arg-gen
  "Argument generation"
  (:require [clojure.test.check
             [generators :as gen]
             [random :as random]
             [rose-tree :as rose]]
            [criterium
             [jvm :as jvm]
             [measure :as measure]
             [measured :as measured]
             [toolkit :as toolkit]]))


;; from c.t.check (private)
(defn- make-rng
  [seed]
  (if seed
    [seed (random/make-random seed)]
    (let [non-nil-seed (jvm/timestamp)]
      [non-nil-seed (random/make-random non-nil-seed)])))

(defn state-fn-state [max-size seed]
  (let [[created-seed rng] (make-rng seed)
        size-seq           (gen/make-size-range-seq max-size)
        ]
    (volatile! {:created-seed created-seed
                :rng rng
                :size-seq size-seq})))

(defn state-fn   ; TODO make state-fn-state a mutable field on measured?
  [gen state-fn-state]
  (fn []
    (let [{:keys [rng size-seq]} @state-fn-state
          [size & rest-size-seq] size-seq
          [r1 r2]                (random/split rng)
          result-map-rose        (gen/call-gen gen r1 size)]
      (vswap! state-fn-state assoc :rng r2 :size-seq rest-size-seq)
      (rose/root result-map-rose))))

(defn measured-impl
  "A function version of `for-all`. Takes a sequence of N generators and a function of N
  args, and returns a measured function, which can be called with generated values, like
  with `for-all`.

  Example:

  (for-all* [gen/large-integer gen/large-integer]
            (fn [a b] (+ a b) a))"
  [gen f {:keys [size seed] :or {size 100 seed nil}}]
  (let [state-fn-state (state-fn-state size seed)
        ;; gen args ;; (apply gen/tuple args)
        ]
    (measured/measured
      (state-fn gen state-fn-state)
      f)))

;; (defn- binding-vars
;;   [bindings]
;;sddcc   (mapv first (partition 2 bindings)))

;; (defn- binding-gens
;;   [bindings]
;;   (mapv second (partition 2 bindings)))

(defmacro measured*
  "Returns a measured, which is the combination of some generators and an expression
  that should be measured for all generated values.

  `for-all` takes a `let`-style bindings vector, where the right-hand side of each
  binding is a generator.

  The body should be an expression of the generated values that will be measured.

  When there are multiple binding pairs, the earlier pairs are visible to the later
  pairs.

  If there are multiple body expressions, all but the last one are executed for side
  effects, as with `do`.

  Example:

  (time*
    (for-all [a gen/large-integer
              b gen/large-integer]
       (+ a b))
    {})"
  [{:keys [size seed arg-metas]
    :or   {size 100 seed nil}
    :as   options}
   bindings & body]
  (let [pairs (partition 2 bindings)
        binding-vars (mapv first pairs)
        binding-gens (reduce
                        (fn [curr [sym code]]
                          `(gen/bind ~code (fn [~sym] ~curr)))
                        `(gen/return ~binding-vars)
                        (reverse pairs))
        options {}
        _ (println "eaxmple-state form"
                   `((state-fn ~binding-gens (state-fn-state 2 nil))))
        example-state (eval `((state-fn ~binding-gens (state-fn-state 2 nil))))
        types (mapv type example-state)
        arg-metas (mapv measured/tag-meta types)
        options {:arg-metas arg-metas}]
    `(measured-impl
       ~binding-gens
       ;; ~(list 'quote `(do ~@body))
       ~(measured/measured-expr-fn
          binding-vars
          `(do ~@body)
          options)
       ~{:size size
         :seed seed}
       ;; (fn ~(gensym "for-all-body") [~binding-vars]
       ;;   ~@body)
       )))



(defmacro measured
  "Return a measured using test.check generators for state."
  ;; ([binding & body]
  ;;  `(measured* nil ~for-all-expr))
  [bindings & body]
  (if (vector? bindings)
    `(measured* nil ~bindings ~@body)
    (do
      (assert (map? bindings) "options must be passed as a literal map")
      `(measured* ~bindings ~@body))))


(comment
  (def m (for-all [i (gen/choose 0 1000000000000000000)]
           (inc i)))

  (def m (for-all [i (gen/choose 0 1000000000000000000)]
           i))

  (def nth-bench
    (for-all [v (gen/vector gen/int 1 100000)
              i (gen/choose 0 (dec (count v)))]
      (nth v i)))

  (def vec-nth-bench
    (for-all [v (gen/vector gen/int 1 100000)
              i (gen/choose 0 (dec (count v)))]
      ;;(.nth ^clojure.lang.APersistentVector v ^int i)
      (.nth  v  i)))

  ((:state-fn m))

  (criterium.measure/measure m {:limit-time 1})
  (dissoc (criterium.measure/measure m {:limit-evals 100}) :samples)
  (dissoc (criterium.measure/measure nth-bench {:limit-time 1}) :samples)
  (dissoc (criterium.measure/measure vec-nth-bench {:limit-time 1}) :samples)
  (criterium.measure/measure m {})
  )

(def g (gen/bind
    (gen/vector gen/int 1 100000)
    (fn [v]
      (gen/bind
        (gen/choose 0 (dec (count v)))
        (fn [i] (gen/return [v i])))))
  )

;; (gen/generate g)
