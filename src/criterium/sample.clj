(ns criterium.sample
  "Sampling functions."
  (:require [criterium
             [measured :as measured]
             [pipeline :as pipeline]
             [toolkit :as toolkit]])
  (:import [criterium.toolkit Budget]))


(defn one-shot
  "Single sample measured with no warmup.
  Forces GC.
  Return a sampled data map."
  [pipeline measured {:keys [max-gc-attempts] :as options}]
  (toolkit/force-gc max-gc-attempts)
  (toolkit/sample
    pipeline
    measured
    (toolkit/budget Long/MAX_VALUE 1)
    1))


(defn quick
  "Sample measured with minimal warmup.
  Does a single warmup followed by forced GC.
  Return a sampled data map."
  [pipeline
   measured
   budget
   {:keys [batch-time-ns max-gc-attempts] :as config}]
  (toolkit/throw-away-sample measured)
  (toolkit/force-gc max-gc-attempts)
  (let [t0         (toolkit/first-estimate measured)
        batch-size (toolkit/estimate-batch-size
                     measured t0 budget batch-time-ns)]
    (toolkit/sample
      pipeline
      measured
      budget
      batch-size)))


(defn full
  "Sample measured with estimation, warmup and forced GC.
  Return a sampled data map."
  [pipeline
   measured
   ^Budget estimation-budget
   ^Budget warmup-budget
   ^Budget sample-budget
   {:keys [max-gc-attempts batch-time-ns]
    :as   config}]
  ;; Start by running GC until it has nothing to do.
  (toolkit/throw-away-sample measured)
  (toolkit/force-gc max-gc-attempts)
  (let [t0         (toolkit/first-estimate measured)
        batch-size (toolkit/estimate-batch-size
                     measured t0 estimation-budget batch-time-ns)
        t1         (toolkit/estimate-execution-time
                     measured
                     estimation-budget
                     batch-size)
        _          (toolkit/force-gc max-gc-attempts)

        batch-size (toolkit/estimate-batch-size
                     measured t1 warmup-budget batch-time-ns)
        samples    (toolkit/warmup
                     measured
                     warmup-budget
                     batch-size)
        t2         (/ (reduce + (map pipeline/elapsed-time samples))
                      (reduce + (map :num-evals samples)))
        _          (toolkit/force-gc max-gc-attempts)

        batch-size  (toolkit/estimate-batch-size
                      measured t2 sample-budget batch-time-ns)
        sample-data (toolkit/sample
                      pipeline
                      measured
                      sample-budget
                      batch-size)]
    sample-data))
