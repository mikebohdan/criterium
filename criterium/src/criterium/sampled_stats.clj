(ns criterium.sampled-stats
  (:require
   [criterium.metric :as metric]
   [criterium.pipeline :as pipeline]
   [criterium.stats :as stats]
   [criterium.util :as util]
   [criterium.well :as well]))


(def ^:private ks
  [:mean :variance :min :median :0.25 :0.75 :lower-tail :upper-tail])

(defn- stats-fns [tail-quantile]
  (juxt
   stats/mean
   stats/variance
   stats/min
   (partial stats/quantile 0.5)
   (partial stats/quantile 0.25)
   (partial stats/quantile 0.75)
   (partial stats/quantile tail-quantile)
   (partial stats/quantile (- 1.0 tail-quantile))))

(defn update-mean-3-sigma [{:keys [mean variance] :as stats} scale-1]
  (let [three-sigma       (* 3.0 (Math/sqrt variance))
        mean-plus-3sigma  (+ mean three-sigma)
        mean-minus-3sigma (- mean three-sigma)]
    (assoc stats
           :mean (scale-1 mean)
           :variance (scale-1 variance)
           :mean-plus-3sigma (scale-1 mean-plus-3sigma)
           :mean-minus-3sigma (scale-1 mean-minus-3sigma))))

(defn sorted-samples-for-path [samples path]
  (->> samples
       (mapv (metric/path-accessor path))
       (mapv double)
       sort))

(defn scale-key-values [stats scale-1 ks]
  (reduce
   (fn [stats k]
     (update stats k scale-1))
   stats
   ks))

(defn stats-for
  [path samples config transforms]
  (let [tail-quantile (:tail-quantile config)
        vs            (sorted-samples-for-path samples path)
        fns           (stats-fns tail-quantile)
        scale-1       (fn [v] (util/transform-sample-> v transforms))]
    (-> (zipmap ks (fns vs))
        (update-mean-3-sigma scale-1)
        (scale-key-values scale-1 (drop 2 ks)))))

(defn sample-stats
  [result metrics samples config]
  (let [paths (mapcat metric/metric-paths metrics)
        stats (reduce
               (fn [res path]
                 (assoc-in
                  res path
                  (stats-for
                   path
                   samples
                   config
                   (util/get-transforms result path))))
               {}
               paths)]

    {:stats       stats
     :num-samples (count samples)
     :metrics     metrics}))

(defn- scale-bootstrap-stat [scale-f stat]
  (-> stat
      (update :point-estimate scale-f)
      (update :estimate-quantiles
              #(mapv (fn [q] (update q :value scale-f)) %))))

(defn update-bootstrap-mean-3-sigma
  [{:keys [mean variance] :as stats} scale-f]
  (let [three-sigma       (* 3 (Math/sqrt (:point-estimate variance)))
        mean-plus-3sigma  (+ (:point-estimate mean) three-sigma)
        mean-minus-3sigma (- (:point-estimate mean) three-sigma)]
    (assoc stats
           :mean (scale-f mean)
           :variance (scale-f variance)
           :mean-plus-3sigma (scale-f
                              {:point-estimate mean-plus-3sigma})
           :mean-minus-3sigma (scale-f
                               {:point-estimate mean-minus-3sigma}))))

(defn scale-key-bootstrap-values [stats f ks]
  (reduce
   (fn [stats k]
     (update stats k f))
   stats
   ks))

(defn bootstrap-stats-for
  [path batch-size samples opts transforms]
  (let [vs            (->>  samples
                            (mapv (metric/path-accessor path))
                            (mapv double))
        tail-quantile (:tail-quantile opts 0.025)
        stats         (stats/bootstrap-bca
                       vs
                       (stats-fns tail-quantile)
                       (:bootstrap-size opts (long (* (count vs) 0.8)))
                       [0.5 tail-quantile (- 1.0 tail-quantile)]
                       well/well-rng-1024a)
        scale-1       (fn [v] (util/transform-sample-> v transforms))
        scale-f       (partial scale-bootstrap-stat scale-1)]
    (-> (zipmap ks stats)
        (update-bootstrap-mean-3-sigma scale-f)
        (scale-key-bootstrap-values scale-f (drop 2 ks)))))

(defn bootstrap-stats
  [result metrics batch-size samples config]
  (let [sum        (pipeline/total samples)
        eval-count (:eval-count sum)
        avg        (pipeline/divide sum eval-count)
        paths      (mapcat metric/metric-paths metrics)
        stats      (reduce
                    (fn [res path]
                      (assoc-in
                       res path
                       (bootstrap-stats-for
                        path
                        batch-size
                        samples
                        config
                        (util/get-transforms result path))))
                    {}
                    paths)]

    {:eval-count      eval-count
     :avg             avg
     :bootstrap-stats stats
     :num-samples     (count samples)
     :batch-size      batch-size
     :metrics         metrics}))

(defn jvm-event-stats
  "Return the stats for JIT compilation and garbage-collection."
  [samples]
  (reduce
   (fn [stats sample]
     (let [update-stat (fn [stat time-ns]
                         (if (pos? time-ns)
                           (-> stat
                               (update :total-time-ms + time-ns)
                               (update :sample-count inc))
                           stat))
           compilation (-> sample :compilation :compilation-time)
           gc          (-> sample :garbage-collector :total :time)]
       (-> stats
           (update :compilation update-stat compilation)
           (update :garbage-collection update-stat gc))))
   {:compilation        {:total-time-ms 0
                         :sample-count  0}
    :garbage-collection {:total-time-ms 0
                         :sample-count  0}}
   samples))
