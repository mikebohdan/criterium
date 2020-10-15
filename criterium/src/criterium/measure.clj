(ns criterium.measure
  "Opinionated measure function."
  (:require [clojure.spec.alpha :as s]
            [criterium
             [budget :as budget]
             [output :as output]
             [pipeline :as pipeline]
             [sample-scheme :as sample-scheme]
             [toolkit :as toolkit]
             [util :as util]])
  (:import [criterium.budget Budget]))

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
   (or (and limit-time-s (* limit-time-s toolkit/SEC-NS))
       (if limit-eval-count
         Long/MAX_VALUE
         DEFAULT-TIME-BUDGET-NS))
   (or limit-eval-count
       Long/MAX_VALUE)))


(defn full-sample-scheme
  [{:keys [limit-time-s limit-eval-count
           estimation-fraction estimation-period-ns
           warmup-fraction warmup-period-ns
           sample-fraction _sample-period-ns
           max-gc-attempts batch-time-ns]
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
     :estimation-budget estimation-budget
     :warmup-budget     warmup-budget
     :sample-budget     (budget/subtract
                         total-budget
                         estimation-budget
                         warmup-budget)
     :max-gc-attempts   max-gc-attempts
     :batch-time-ns     batch-time-ns}))

(defn one-shot-sample-scheme
  [{:keys [max-gc-attempts]
    :as   _options}]
  {:scheme-type     :one-shot
   :max-gc-attempts (or max-gc-attempts 3)})

(def default-options
  "Default options for criterium.measure."
  {:verbose       false
   :pipeline      {:stages     []
                   :terminator :elapsed-time-ns}
   :sample-scheme (full-sample-scheme {})
   :analysis      [:stats]})

(s/def ::verbose boolean?)
(s/def ::stages ::pipeline/pipeline-fn-kws)
(s/def ::terminator ::pipeline/terminal-fn-kw)
(s/def ::pipeline (s/keys ::req-un [::stages ::terminator]))
(s/def ::analysis (s/coll-of keyword?))
(s/def ::options (s/keys :req-un [::verbose
                                  ::pipeline
                                  ::analysis
                                  ::sample-scheme/sample-scheme
                                  ]))

(defn- default-analysis [{:keys [sample-scheme] :as options-map}]
  (if (= :one-shot (:scheme-type sample-scheme))
    [:samples]
    [:stats]))


(defn ensure-pipeline-stages
  "Add any injected stages that aren't already present."
  [{:keys [pipeline sample-scheme] :as options}]
  (let [stages (set (:stages pipeline))
        injected (sample-scheme/required-stages sample-scheme)]
    (update-in
     options
     [:pipeline :stages]
     (fnil into [])
     (remove stages injected))))


(defn expand-options
  "Convert option arguments into a criterium option map.
  The options map specifies how criterium will execute."
  [options-map]
  (-> (util/deep-merge
      (merge
       default-options
       (cond-> {}
         (not (:analysis options-map))
         (assoc :analysis (default-analysis options-map))))
      options-map)
      ensure-pipeline-stages))

;; (defn- pipeline-spec
;;   "Return a pipeline spec for the given user specified options.
;;   Recognizes :pipeline-fn-kws and terminal-fn-kw options.
;;   Specifying :all for :pipeline-fn-kws uses all known pipeline functions."
;;   [options]
;;   (let [pipeline-fn-kws (let [pipeline-opt (:pipeline-fn-kws options [])]
;;                           (if (= :all pipeline-opt)
;;                             (keys pipeline/pipeline-fns)
;;                             pipeline-opt))
;;         terminal-fn-kw  (get options :terminal-fn-kw :elapsed-time-ns)]
;;     {:pipeline-fn-kws pipeline-fn-kws
;;      :terminal-fn-kw  terminal-fn-kw}))


(defn- pipeline-from-options
  [{:keys [pipeline] :as _spec}]
  (pipeline/pipeline
   (:stages pipeline)
   (:terminator pipeline)))

;; (defmulti sample-data
;;   #_{:clj-kondo/ignore [:unused-binding]}
;;   (fn [sample-scheme measured options]
;;     (:sample-mode sample-scheme)))

;; (defmethod sample-data :one-shot
;;   [_ measured options]
;;   (let [pipeline (pipeline-from-options options)]
;;     ;; TODo consider warming up criterium code
;;     (sample/one-shot pipeline measured options)))

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

;; (defmethod sample-data :full
;;   [{:keys [estimation-budget warmup-budget sample-budget]}
;;    measured
;;    options]
;;   (let [pipeline (pipeline-from-options options)]
;;     (output/progress
;;      "estimation-budget" estimation-budget)
;;     (output/progress
;;      "warmup-budget" warmup-budget)
;;     (output/progress
;;      "sample-budget" sample-budget)
;;     (sample/full
;;      pipeline
;;      measured
;;      estimation-budget
;;      warmup-budget
;;      sample-budget
;;      options)))


(defn measure
  [measured
   {:keys [pipeline sample-scheme analysis] :as  options}]

  ;; {:pre [(s/valid? ::options options)]}
  (output/progress "options:   " options)
  (assert (s/valid? ::options options) (s/explain ::options options))
  (output/progress "sample-scheme:   " sample-scheme)
  (output/progress "analysis:  " analysis)
  (let [pipeline (pipeline-from-options options)
        sampled         (sample-scheme/sample
                         pipeline
                         measured
                         sample-scheme)]
    sampled))
