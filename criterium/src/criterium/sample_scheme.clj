(ns criterium.sample-scheme
  "Sample schemes to control the collection of samples from a measured."
  (:require
   [clojure.spec.alpha :as s]
   [criterium.budget :as budget]
   [criterium.domain :as domain]
   [criterium.measured :as measured]
   [criterium.output :as output]
   [criterium.pipeline :as pipeline]
   [criterium.toolkit :as toolkit]))

(defmulti required-stages
  "Pipeline stages required for the given schema-type"
  (fn [{:keys [scheme-type] :as _sample-scheme}]
    scheme-type))

(defmethod required-stages :one-shot
  [_sample-scheme]
  [])

(defmethod required-stages :full
  [_sample-scheme]
  [:compilation-time
   :garbage-collector])

(defmulti sample
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [pipeline measured config]
    (:scheme-type config)))

(defmethod sample :one-shot
  ;; Collects a Single sample measured with no warmup of the measured function.
  ;; Forces GC.
  ;; Return a sampled data map.
  [pipeline measured {:keys [max-gc-attempts] :as _config}]
  (toolkit/force-gc max-gc-attempts)
  (let [sample (pipeline/execute pipeline measured 1)]
    {:batch-size      1
     :elapsed-time-ns (pipeline/elapsed-time sample)
     :eval-count      1
     :samples         [sample]}))

(defmethod sample :full
  ;; Sample measured with estimation, warmup and forced GC.
  ;; Return a sampled data map.
  [pipeline
   measured
   {:keys [max-gc-attempts
           batch-time-ns
           estimation-budget
           warmup-budget
           sample-budget]
    :as   config}]
  {:pre [pipeline
         (measured/measured? measured)
         (s/valid? ::full-config config)]}
  ;; Start by running GC until it has nothing to do.
  (toolkit/throw-away-sample measured)
  (toolkit/force-gc max-gc-attempts)
  (let [t0         (toolkit/first-estimate measured)
        batch-size (toolkit/estimate-batch-size
                    t0 estimation-budget batch-time-ns)
        t1         (toolkit/estimate-execution-time
                    measured
                    estimation-budget
                    batch-size)
        _          (toolkit/force-gc max-gc-attempts)

        batch-size (toolkit/estimate-batch-size
                    t1 warmup-budget batch-time-ns)
        {:keys [elapsed-time-ns eval-count] :as warmup-data}
        (toolkit/warmup
         measured
         warmup-budget
         batch-size)
        t2         (max 1 (long (/ elapsed-time-ns eval-count)))
        _          (toolkit/force-gc max-gc-attempts)

        batch-size (toolkit/estimate-batch-size
                    t2 sample-budget batch-time-ns)
        _          (output/progress "Batch-size:" batch-size
                                    t2 sample-budget batch-time-ns)

        sample-data   (toolkit/sample
                       pipeline
                       measured
                       sample-budget
                       batch-size)
        final-gc-data (toolkit/force-gc max-gc-attempts)]
    (assoc sample-data
           :warmup warmup-data
           :final-gc final-gc-data)))

(s/def ::scheme-type #{:one-shot :full})

(s/def ::max-gc-attempts nat-int?)

(s/def ::batch-time-ns ::domain/elapsed-time-ns)

(s/def ::estimation-budget ::budget/budget)
(s/def ::warmup-budget ::budget/budget)
(s/def ::sample-budget ::budget/budget)

(s/def ::full-config
  (s/keys
   :req-un
   [::scheme-type
    ::estimation-budget
    ::warmup-budget
    ::sample-budget
    ::max-gc-attempts
    ::batch-time-ns]))

(s/def ::one-shot-config (s/keys :req-un [::scheme-type ::max-gc-attempts]))

(s/def ::sample-scheme (s/or :full-config ::one-shot-config))

(s/def ::samples (s/coll-of ::pipeline/sample))

(s/def ::sample-result (s/keys :req-un
                               [::domain/batch-size
                                ::domain/elapsed-time-ns
                                ::domain/eval-count
                                ::samplea]))

(s/fdef sample
  :args (s/cat :pipeline ::pipeline/pipeline-fn
               :measured ::measured/measured
               :scheme ::sample-scheme)
  :ret ::sample-result)
