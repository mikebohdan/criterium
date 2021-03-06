(ns criterium.sampled-stats
  (:require
   [criterium.metric :as metric]
   [criterium.pipeline :as pipeline]
   [criterium.stats :as stats]
   [criterium.well :as well]))


(def ^:private ks
  [:min :mean :variance :median :0.25 :0.75 :lower-tail :upper-tail])

(defn- stats-fns [tail-quantile]
  (juxt
   stats/min
   stats/mean
   stats/variance
   (partial stats/quantile 0.5)
   (partial stats/quantile 0.25)
   (partial stats/quantile 0.75)
   (partial stats/quantile tail-quantile)
   (partial stats/quantile (- 1.0 tail-quantile))))

(defn stats-for [path batch-size samples opts]
  (let [vs             (->> samples
                            (mapv (metric/path-accessor path))
                            (mapv double)
                            sort)
        tail-quantile  (:tail-quantile opts 0.025) ; TODO pull out default into config
        stats          ((stats-fns tail-quantile) vs)
        sqr-batch-size (stats/sqr batch-size)
        scale-1        (fn [v] (/ v batch-size))
        scale-2        (fn [v] (/ v sqr-batch-size))
        scale-fns      {:variance scale-2}
        stats          (zipmap ks
                               (mapv
                                (fn [k stat]
                                  ((scale-fns k scale-1) stat))
                                ks
                                stats))]
    stats))

(defn sample-stats [metrics batch-size samples config]
  (let [sum        (pipeline/total samples)
        eval-count (:eval-count sum)
        avg        (pipeline/divide sum eval-count)
        paths      (mapcat metric/metric-paths metrics)
        stats      (reduce
                    (fn [res path]
                      (assoc-in
                       res path
                       (stats-for path batch-size samples config)))
                    {}
                    paths)]

    {:eval-count  eval-count
     :avg         avg
     :stats       stats
     :num-samples (count samples)
     :batch-size  batch-size
     :metrics     metrics}))

(defn bootstrap-stats-for [path batch-size samples opts]
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

        sqr-batch-size (stats/sqr batch-size)
        scale-1        (fn [[p [l u]]]
                         [(/ p batch-size)
                          [(/ l batch-size) (/ u batch-size)]])
        ;; TODO check this
        scale-2        (fn [[p [l u]]]
                         [(/ p sqr-batch-size)
                          [(/ l sqr-batch-size) (/ u sqr-batch-size)]])
        scale-fns      {:variance scale-2}
        stats          (zipmap ks
                               (mapv
                                (fn [k stat]
                                  ((scale-fns k scale-1) stat))
                                ks
                                stats))]
    stats))

(defn bootstrap-stats [metrics batch-size samples config]
  (let [sum        (pipeline/total samples)
        eval-count (:eval-count sum)
        avg        (pipeline/divide sum eval-count)
        paths      (mapcat metric/metric-paths metrics)
        stats      (reduce
                    (fn [res path]
                      (assoc-in
                       res path
                       (bootstrap-stats-for path batch-size samples config)))
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
