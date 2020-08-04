(ns criterium.measure
  "Opinionated measure function."
  (:require [criterium
             [jvm :as jvm]
             [measured :as measured]
             [pipeline :as pipeline]
             [toolkit :as toolkit]
             [sample :as sample]])
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
  ^Budget [limit-time-s limit-eval-count]
  (toolkit/budget
    (or limit-time-s DEFAULT-TIME-BUDGET-NS)
    (or limit-eval-count DEFAULT-EVAL-COUNT-BUDGET)))

(defmulti sample-data
  (fn [sample-mode pipeline measured total-budget config options]
    sample-mode))

(defmethod sample-data :one-shot
  [_ pipeline measured total-budget config _options]
  (sample/one-shot pipeline measured config))

(defmethod sample-data :quick
  [_ pipeline measured total-budget config
   {:keys [estimation-fraction estimation-period-ns
           warmup-fraction warmup-period-ns
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
        options (select-keys options [:terminal-fn])]
    (pipeline/pipeline kws options)))

(defmulti report-sampled
  (fn [report-mode sampled options]
    report-mode))

(defmethod report-sampled :samples
  [report-mode sampled options]
  sampled)

(defn measure
  [measured
   {:keys [sample-mode
           limit-time-s limit-eval-count
           max-gc-attempts
           batch-time-ns
           report-mode]
    :as   options}]
  (let [sample-mode  (or sample-mode
                         (if (or limit-time-s limit-eval-count)
                           :full
                           :quick))
        report-mode  (or report-mode
                         (if (or limit-time-s limit-eval-count)
                           :interval
                           :point))
        config       {:max-gc-attempts (or max-gc-attempts 3)
                      :batch-time-ns   (or batch-time-ns
                                           DEFAULT-BATCH-TIME-NS)}
        total-budget (budget-for-limits limit-time-s limit-eval-count)

        pipeline (pipeline options)

        sampled (sample-data
                  sample-mode pipeline measured total-budget config options)]
    (println :total-budget total-budget
             (.elapsed-time-ns total-budget)
             (.eval-count total-budget))
    (report-sampled report-mode sampled options)))

(measure
  (measured/expr 1)
  {:report-mode :samples
   :sample-mode :quick})
