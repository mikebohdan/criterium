(ns criterium.sample
  "Sampling functions."
  (:require [criterium
             [measured :as measured]
             [output :as output]
             [pipeline :as pipeline]
             [toolkit :as toolkit]])
  (:import [criterium.toolkit Budget]))


(defn one-shot
  "Single sample measured with no warmup.
  Forces GC.
  Return a sampled data map."
  [pipeline measured {:keys [max-gc-attempts] :as _config}]
  (toolkit/force-gc max-gc-attempts)
  (pipeline/execute pipeline measured 1))


(defn quick
  "Sample measured with minimal warmup.
  Does a single warmup followed by forced GC.
  Return a sampled data map."
  [pipeline
   measured
   estimation-budget
   sample-budget
   {:keys [batch-time-ns max-gc-attempts] :as _config}]
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
                     measured t1 sample-budget batch-time-ns)]
    (toolkit/force-gc max-gc-attempts)
    (toolkit/sample
      pipeline
      measured
      sample-budget
      batch-size)))


(defn full
  "Sample measured with estimation, warmup and forced GC.
  Return a sampled data map."
  [pipeline
   measured
   estimation-budget
   warmup-budget
   sample-budget
   {:keys [max-gc-attempts batch-time-ns]
    :as   _config}]
  {:pre [pipeline]}
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
        {:keys [elapsed-time-ns eval-count]}
        (toolkit/warmup
          measured
          warmup-budget
          batch-size)
        t2         (long (/ elapsed-time-ns eval-count))
        _          (toolkit/force-gc max-gc-attempts)


        batch-size  (toolkit/estimate-batch-size
                     measured t2 sample-budget batch-time-ns)
        _           (output/progress "Batch-size:" batch-size
                                     t2 sample-budget batch-time-ns)

        sample-data (toolkit/sample
                      pipeline
                      measured
                      sample-budget
                      batch-size)]
    sample-data))
