(ns criterium.measure
  "Opinionated measure function."
  (:require [criterium
             [measured :as measured]
             [pipeline :as pipeline]
             [toolkit :as toolkit]
             [sample :as sample]
             [sampled-stats :as sampled-stats]])
  (:import [criterium.toolkit Budget]))


(def ^Long DEFAULT-TIME-BUDGET-NS
  "Default time budget when no limit specified.
  100ms should be imperceptible."
  (* 100 ^long toolkit/MILLISEC-NS))

(def ^Long DEFAULT-EVAL-COUNT-BUDGET
  "Default eval count budget when no limit specified."
  10000)

(def ^Long DEFAULT-BATCH-TIME-NS
  (* 10 toolkit/MICROSEC-NS))

(def ^Double DEFAULT-ESTIMATION-FRACTION 0.1)
(def ^Double DEFAULT-WARMUP-FRACTION 0.2)

(defn budget-for-limits
  ^Budget [limit-time-s limit-eval-count factor]
  (toolkit/budget
    (or limit-time-s (* factor DEFAULT-TIME-BUDGET-NS))
    (or limit-eval-count (* factor DEFAULT-EVAL-COUNT-BUDGET))))

(defmulti sample-data
  (fn [sample-mode _pipeline _measured _total-budget _config _options]
    sample-mode))

(defmethod sample-data :one-shot
  [_ pipeline measured _total-budget config _options]
  (sample/one-shot pipeline measured config))

(defmethod sample-data :quick
  [_ pipeline measured total-budget config
   {:keys [estimation-fraction estimation-period-ns
           sample-fraction sample-period-ns]
    :as   options}]
  (let [^double estimation-frac (or estimation-fraction
                                    DEFAULT-ESTIMATION-FRACTION)
        ^double sample-frac     (if sample-fraction
                                  sample-fraction
                                  (- 1.0 estimation-frac))
        estimation-budget       (toolkit/phase-budget
                                  total-budget
                                  estimation-period-ns
                                  estimation-fraction
                                  estimation-frac)
        sample-budget           (toolkit/reduce-budget
                                  estimation-budget)]
    (sample/quick pipeline measured estimation-budget sample-budget config)))

(defmethod sample-data :full
  [_ pipeline measured total-budget config
   {:keys [estimation-fraction estimation-period-ns
           warmup-fraction warmup-period-ns
           sample-fraction sample-period-ns]
    :as   options}]
  (let [estimation-frac (or estimation-fraction
                            DEFAULT-ESTIMATION-FRACTION)
        warmup-frac     (or warmup-fraction
                            DEFAULT-WARMUP-FRACTION)
        sample-frac     (or sample-fraction
                            (- 1.0
                               estimation-frac
                               warmup-frac))

        estimation-budget (toolkit/phase-budget
                            total-budget
                            estimation-period-ns
                            estimation-fraction
                            estimation-frac)
        warmup-budget     (toolkit/phase-budget
                            total-budget
                            warmup-period-ns
                            warmup-fraction
                            warmup-frac)
        sample-budget     (toolkit/reduce-budget
                           estimation-budget
                           warmup-budget)]
    (sample/full
      pipeline
      measured
      estimation-budget
      warmup-budget
      sample-budget
      config)))


(defn- pipeline [options]
  (let [kws     (let [pipeline-opt (:pipeline options [])]
                  (if (= :all pipeline-opt)
                    (keys pipeline/pipeline-fns)
                    pipeline-opt))
        terminal-kw (get options :terminal :elapsed-time-ns)
        options {:terminal-fn (pipeline/terminal-fns terminal-kw)}
        metrics (conj kws terminal-kw)]
    [(pipeline/pipeline kws options) metrics]))

(defmulti process-samples
  (fn [process-mode sampled metrics options]
    process-mode))

(defmethod process-samples :samples
  ;; mode to just return the samples
  [process-mode sampled metrics options]
  sampled)

(defmethod process-samples :stats
  ;; mode to just return the samples
  [process-mode sampled metrics options]
  (let [])
  (sampled-stats/sample-stats
    metrics
    (:batch-size sampled)
    (:samples sampled)
    options))

(defn measure
  [measured
   {:keys [sample-mode
           limit-time-s limit-eval-count
           max-gc-attempts
           batch-time-ns
           process-mode]
    :as   options}]
  (let [sample-mode     (or sample-mode
                            (if (or limit-time-s limit-eval-count)
                              :full
                              :quick))
        process-mode    (or process-mode
                            (if (= :one-shot sample-mode)
                              :samples
                              :stats))
        config          {:max-gc-attempts (or max-gc-attempts 3)
                         :batch-time-ns   (or batch-time-ns
                                              (* 10 DEFAULT-BATCH-TIME-NS))}
        factor          (if (= sample-mode :quick) 1 10)
        total-budget    (budget-for-limits limit-time-s limit-eval-count factor)
        [pline metrics] (pipeline options)
        sampled         (sample-data
                          sample-mode pline measured total-budget config options)]
    (process-samples process-mode sampled metrics options)))