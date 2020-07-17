(ns criterium.arg-gen
  "Argument generation"
  (:require [clojure.test.check
             [generators :as gen]
             [random :as random]
             [rose-tree :as rose]]
            [criterium
             [jvm :as jvm]
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

(defn state-fn   ;; TODO make state-fn-state a mutable field on measured?
  [gen state-fn-state]
  (fn []
    (let [{:keys [rng size-seq]} @state-fn-state
          [size & rest-size-seq] size-seq
          [r1 r2]                (random/split rng)
          result-map-rose        (gen/call-gen gen r1 size)]
      (vswap! state-fn-state assoc :rng r2 :size-seq rest-size-seq)
      (rose/root result-map-rose))))

(defn for-all*
  "A function version of `for-all`. Takes a sequence of N generators and a function of N
  args, and returns a measured function, which can be called with generated values, like
  with `for-all`.

  Example:

  (for-all* [gen/large-integer gen/large-integer]
            (fn [a b] (+ a b) a))"
  [args f]
  (let [size 100
        seed nil
        state-fn-state (state-fn-state size seed)
        gen args ;; (apply gen/tuple args)
        ]
    (toolkit/measured
      (state-fn gen state-fn-state)
      f
      1)))

;; (defn- binding-vars
;;   [bindings]
;;   (mapv first (partition 2 bindings)))

;; (defn- binding-gens
;;   [bindings]
;;   (mapv second (partition 2 bindings)))

(defmacro for-all
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
  [bindings & body]
  (let [pairs (partition 2 bindings)
        binding-vars (mapv first pairs)
        binding-gens (reduce
                        (fn [curr [sym code]]
                          `(gen/bind ~code (fn [~sym] ~curr)))
                        `(gen/return ~binding-vars)
                        (reverse pairs))]
    `(for-all*
       ~binding-gens
       (fn ~(gensym "for-all-body") [~binding-vars]
         ~@body))))

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
      (.nth ^clojure.lang.APersistentVector v ^int i)))

  ((:state-fn m))

  (criterium.time/measure* m {:limit-time 1})
  (dissoc (criterium.time/measure* m {:limit-evals 100}) :samples)
  (dissoc (criterium.time/measure* nth-bench {:limit-time 1}) :samples)
  (dissoc (criterium.time/measure* vec-nth-bench {:limit-time 1}) :samples)
  (criterium.time/measure* m {})
  )

(def g (gen/bind
    (gen/vector gen/int 1 100000)
    (fn [v]
      (gen/bind
        (gen/choose 0 (dec (count v)))
        (fn [i] (gen/return [v i])))))
  )

;; (gen/generate g)
