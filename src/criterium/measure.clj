(ns criterium.measure
  "Opinionated measure function."
  (:require [criterium
             [budget :as budget]
             [format :as format]
             [output :as output]
             [pipeline :as pipeline]
             [toolkit :as toolkit]
             [sample :as sample]
             [sampled-stats :as sampled-stats]])
  (:import [criterium.budget Budget]))


(def ^Long DEFAULT-TIME-BUDGET-NS
  "Default time budget when no limit specified.
  100ms should be imperceptible."
  (* 100 ^long toolkit/MILLISEC-NS))

;; (def ^Long DEFAULT-EVAL-COUNT-BUDGET
;;   "Default eval count budget when no limit specified."
;;   1000000)

(def ^Long DEFAULT-BATCH-TIME-NS
  (* 10 toolkit/MICROSEC-NS))

(def ^Double DEFAULT-ESTIMATION-FRACTION 0.1)
(def ^Double DEFAULT-WARMUP-FRACTION 0.8)

(defn budget-for-limits
  ^Budget [limit-time-s limit-eval-count factor]
  (budget/budget
    (or (and limit-time-s (* limit-time-s toolkit/SEC-NS))
        (if limit-eval-count
          Long/MAX_VALUE
          (* factor DEFAULT-TIME-BUDGET-NS)))
    (or limit-eval-count
        Long/MAX_VALUE
        #_(if limit-time-s
          Long/MAX_VALUE
          (* factor DEFAULT-EVAL-COUNT-BUDGET)))))



(defn- pipeline-spec
  "Return a pipeline spec for the given user specified options.
  Recognizes :pipeline-fn-kws and terminal-fn-kw options.
  Specifying :all for :pipeline-fn-kws uses all known pipeline functions."
  [options]
  (let [pipeline-fn-kws (let [pipeline-opt (:pipeline-fn-kws options [])]
                          (if (= :all pipeline-opt)
                            (keys pipeline/pipeline-fns)
                            pipeline-opt))
        terminal-fn-kw  (get options :terminal-fn-kw :elapsed-time-ns)]
    {:pipeline-fn-kws pipeline-fn-kws
     :terminal-fn-kw  terminal-fn-kw}))

(defn pipeline-metrics
  "Return a sequence of all metrics produced by a pipeline with the
  given spec."
  [{:keys [pipeline-fn-kws terminal-fn-kw] :as _spec}]
  (conj pipeline-fn-kws terminal-fn-kw))

(defn- pipeline-from-spec
  [{:keys [pipeline-fn-kws terminal-fn-kw] :as _spec}
   extra-pipeline-fn-kws]
  (pipeline/pipeline
    (into pipeline-fn-kws extra-pipeline-fn-kws)
    {:terminal-fn-kw terminal-fn-kw}))

;; (defn- pipeline [options]
;;   (let [kws     (let [pipeline-opt (:pipeline options [])]
;;                   (if (= :all pipeline-opt)
;;                     (keys pipeline/pipeline-fns)
;;                     pipeline-opt))
;;         terminal-kw (get options :terminal :elapsed-time-ns)
;;         options {:terminal-fn (pipeline/terminal-fns terminal-kw)}
;;         metrics (conj kws terminal-kw)]
;;     [(pipeline/pipeline kws options) metrics]))


(defmulti sample-data
  (fn [sample-mode _pipeline-spec _measured _total-budget _config _options]
    sample-mode))

(defmethod sample-data :one-shot
  [_ pipeline-spec measured _total-budget config _options]
  (let [pipeline (pipeline-from-spec pipeline-spec [])]
    (sample/one-shot pipeline measured config)))

;; (defmethod sample-data :quick
;;   [_ pipeline measured total-budget config
;;    {:keys [estimation-fraction estimation-period-ns
;;            sample-fraction sample-period-ns]
;;     :as   options}]
;;   (let [^double estimation-frac (or estimation-fraction
;;                                     DEFAULT-ESTIMATION-FRACTION)
;;         ^double sample-frac     (if sample-fraction
;;                                   sample-fraction
;;                                   (- 1.0 estimation-frac))
;;         estimation-budget       (toolkit/phase-budget
;;                                   total-budget
;;                                   estimation-period-ns
;;                                   estimation-fraction
;;                                   estimation-frac)
;;         sample-budget           (toolkit/reduce-budget
;;                                   total-budget
;;                                   estimation-budget)]
;;     (sample/quick pipeline measured estimation-budget sample-budget config)))

(defmethod sample-data :full
  [_ pipeline-spec measured total-budget config
   {:keys [estimation-fraction estimation-period-ns
           warmup-fraction warmup-period-ns
           sample-fraction sample-period-ns]
    :as   options}]
  (let [pipeline        (pipeline-from-spec
                          pipeline-spec
                          [:compilation-time
                           :garbage-collector])
        estimation-frac (or estimation-fraction
                            DEFAULT-ESTIMATION-FRACTION)
        warmup-frac     (or warmup-fraction
                            DEFAULT-WARMUP-FRACTION)
        sample-frac     (or sample-fraction
                            (- 1.0
                               estimation-frac
                               warmup-frac))

        estimation-budget (budget/phase-budget
                            total-budget
                            estimation-period-ns
                            estimation-fraction
                            estimation-frac)
        warmup-budget     (budget/phase-budget
                            total-budget
                            warmup-period-ns
                            warmup-fraction
                            warmup-frac)
        sample-budget     (budget/subtract
                            total-budget
                            estimation-budget
                            warmup-budget)]
    (output/progress
      "total-budget" total-budget)
    (output/progress
      "estimation-budget" estimation-budget)
    (output/progress
      "warmup-budget" warmup-budget)
    (output/progress
      "sample-budget" sample-budget)
    (sample/full
      pipeline
      measured
      estimation-budget
      warmup-budget
      sample-budget
      config)))


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
  (output/progress "Num samples" (count (:samples sampled)))
  (let [res (sampled-stats/sample-stats
              metrics
              (:batch-size sampled)
              (:samples sampled)
              options)
        res  (assoc
               res
               :jvm-event-stats
               (sampled-stats/jvm-event-stats (:samples sampled)))]
    (if (or (:histogram options) (:include-samples options))
      (assoc res :samples (:samples sampled))
      res)))

(defn measure
  [measured
   {:keys [sample-mode
           limit-time-s limit-eval-count
           max-gc-attempts
           batch-time-ns
           process-mode]
    :as   options}]
  (let [sample-mode     (or sample-mode
                            :full
                            ;; (if (or limit-time-s limit-eval-count)
                            ;;   :full
                            ;;   :quick)
                            )
        process-mode    (or process-mode
                            (if (= :one-shot sample-mode)
                              :samples
                              :stats))
        _ (output/progress "Modes:  " sample-mode " process" process-mode)
        config          {:max-gc-attempts (or max-gc-attempts 3)
                         :batch-time-ns   (or batch-time-ns
                                              (* 10 DEFAULT-BATCH-TIME-NS))}
        factor          1 ;; (if (= sample-mode :quick) 1 100)
        total-budget    (budget-for-limits limit-time-s limit-eval-count factor)
        _ (output/progress
            "Limits:  time"
            (format/format-value :time-ns (.elapsed-time-ns total-budget))
            " evaluations"
            (format/format-value :count (.eval-count total-budget)))
        pipeline-spec   (pipeline-spec options)
        metrics         (pipeline-metrics pipeline-spec)
        sampled         (sample-data
                          sample-mode
                          pipeline-spec
                          measured
                          total-budget
                          config
                          options)]
    (process-samples process-mode sampled metrics options)))
