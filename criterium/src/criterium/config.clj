(ns criterium.config
  "Configuration builders"
  (:require
   [clojure.spec.alpha :as s]
   [criterium.analyze :as analyze]
   [criterium.budget :as budget]
   [criterium.pipeline :as pipeline]
   [criterium.report :as report]
   [criterium.sample-scheme :as sample-scheme]
   [criterium.toolkit :as toolkit]
   [criterium.util :as util])
  (:import
   [criterium.budget Budget]))

(def ^Long DEFAULT-TIME-BUDGET-NS
  "Default time budget when no limit specified.
  100ms should be imperceptible."
  (* 100 ^long toolkit/MILLISEC-NS))

;; (def ^Long DEFAULT-EVAL-COUNT-BUDGET
;;   "Default eval count budget when no limit specified."
;;   1000000)

(def ^Long DEFAULT-BATCH-TIME-NS
  (* 100 toolkit/MICROSEC-NS))

(def ^Double DEFAULT-ESTIMATION-FRACTION 0.1)
(def ^Double DEFAULT-WARMUP-FRACTION 0.8)

(defn budget-for-limits
  ^Budget [limit-time-s limit-eval-count]
  (budget/budget
   (or (and limit-time-s (long (* limit-time-s toolkit/SEC-NS)))
       (if limit-eval-count
         Long/MAX_VALUE
         DEFAULT-TIME-BUDGET-NS))
   (or limit-eval-count
       Long/MAX_VALUE)))

(defn full-sample-scheme
  [{:keys [batch-time-ns
           estimation-fraction
           estimation-period-ns
           limit-eval-count
           limit-time-s
           max-gc-attempts
           thread-priority
           warmup-fraction
           warmup-period-ns]
    :as   _options}]
  (let [total-budget      (budget-for-limits limit-time-s limit-eval-count)
        estimation-frac   (or estimation-fraction
                              DEFAULT-ESTIMATION-FRACTION)
        warmup-frac       (or warmup-fraction
                              DEFAULT-WARMUP-FRACTION)
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
        max-gc-attempts   (or max-gc-attempts 3)
        batch-time-ns     (or batch-time-ns DEFAULT-BATCH-TIME-NS)]

    {:scheme-type       :full
     :batch-time-ns     batch-time-ns
     :estimation-budget estimation-budget
     :max-gc-attempts   max-gc-attempts
     :sample-budget     (budget/subtract
                         total-budget
                         estimation-budget
                         warmup-budget)
     :thread-priority   thread-priority
     :warmup-budget     warmup-budget}))

(defn one-shot-sample-scheme
  [{:keys [max-gc-attempts]
    :as   _options}]
  {:scheme-type     :one-shot
   :max-gc-attempts (or max-gc-attempts 3)})

(def default-config
  "Default options for criterium.measure."
  {:verbose       false
   :pipeline      {:stages     []
                   :terminator :elapsed-time-ns}
   :sample-scheme (full-sample-scheme {})
   :analysis      [{:analysis-type  :stats
                    :tail-quantile  0.025
                    :bootstrap-size 100}]
   :report        [{:report-type :stats}
                   {:report-type :jvm-event-stats}
                   {:report-type :final-gc-warnings}]
   :return-value  :expr-value})

(s/def ::verbose boolean?)
(s/def ::pipeline ::pipeline/pipeline-config)
(s/def ::analysis ::analyze/analysis-config)
(s/def ::report ::report/report-config)
(s/def ::config (s/keys :req-un [::verbose
                                 ::pipeline
                                 ::analysis
                                 ::sample-scheme/sample-scheme
                                 ::report
                                 ::return-value]))

;;; analysis stages

(defn analyze-stats
  "Add simple sample stats to the result."
  {:arglists '([][{:keys [tail-quartile]}])}
  ([] (analyze-stats {}))
  ([options]
   (merge
    {:analysis-type :stats
     :tail-quantile 0.025}
    options)))

(defn analyze-bootstrap-stats
  "Add simple bootstrap sample stats to the result."
  {:arglists '([][{:keys [tail-quartile bootstrap-size]}])}
  ([] (analyze-stats {}))
  ([options]
   (merge
    {:analysis-type  :bootstrap-stats
     :tail-quantile  0.025
     :bootstrap-size nil}
    options)))

(defn analyze-samples
  "Add the samples to the result on the :samples key."
  []
  {:analysis-type :samples})

(defn default-analysis-full
  []
  [(analyze-stats)])

(defn default-analysis-one-shot
  []
  [(analyze-samples)])

;;; report stages

(defn report-stats
  "Print sample stats for each metric."
  {:arglists '([][{:keys []}])}
  ([] (report-stats {}))
  ([options]
   (merge
    {:report-type :stats}
    options)))

(defn report-metrics
  "Print values for each metric."
  ([] (report-stats {}))
  ([options]
   (merge
    {:report-type :metrics}
    options)))

;;; options builder

(defn- add-metrics
  "Add any injected stages that aren't already present."
  [{:keys [pipeline sample-scheme] :as config}]
  (assoc config :metrics (pipeline/metrics (:pipeline config))))

(defn ensure-pipeline-stages
  "Add any injected stages that aren't already present."
  [{:keys [pipeline sample-scheme] :as options}]
  (let [stages   (set (:stages pipeline))
        injected (sample-scheme/required-stages sample-scheme)]
    (update-in
     options
     [:pipeline :stages]
     (fnil into [])
     (remove stages injected))))

(defn expand-options
  "Convert option arguments into a criterium config map.
  The config map specifies how criterium will execute."
  [options-map]
  (-> (util/deep-merge
       (merge
        default-config
        (cond-> {}
          (not (:analysis options-map))
          (assoc
           :analysis
           (if (= :one-shot (some-> options-map :sample-scheme :scheme-type))
             (default-analysis-one-shot)
             (default-analysis-full)))

          (:sample-scheme options-map)
          (assoc :sample-scheme nil)))
       options-map)
      add-metrics
      ensure-pipeline-stages))
