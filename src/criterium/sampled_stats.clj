(ns criterium.sampled-stats
  (:require [criterium
             [metric :as metric]
             [pipeline :as pipeline]
             [stats :as stats]
             [well :as well]]))

(defn stats-for [path batch-size samples opts]
  (let [vs            (mapv (metric/path-accessor path) samples)
        tail-quantile (:tail-quantile opts 0.025)
        ;; _ (println "path" path "vs" vs)
        stats         (stats/bootstrap-bca
                        (mapv double vs)
                        (juxt
                          stats/mean
                          stats/variance
                          (partial stats/quantile 0.5)
                          (partial stats/quantile tail-quantile)
                          (partial stats/quantile (- 1.0 tail-quantile)))
                        (:bootstrap-size opts 100)
                        [0.5 tail-quantile (- 1.0 tail-quantile)]
                        well/well-rng-1024a)
        ks [:mean :variance :median :0.025 :0.975 ]
        sqr-batch-size (stats/sqr batch-size)
        scale-1 (fn [[p [l u]]]
                   [(/ p batch-size)
                    [(/ l batch-size) (/ u batch-size)]])
        scale-2 (fn [[p [l u]]]
                   [(/ p sqr-batch-size)
                    [(/ l sqr-batch-size) (/ u sqr-batch-size)]]) ;; TODO FIXME
        scale-fns {:mean scale-1
                   :variance scale-2
                   :median scale-1
                   :0.025 scale-1
                   :0.975 scale-1}

        stats (zipmap ks stats)
        stats (zipmap ks
                      (mapv
                        (fn [k]
                          ((scale-fns k) (k stats)))
                        ks))]
    stats))

(defn sample-stats [metrics batch-size samples {:keys [return-samples] :as opts}]
  ;; (clojure.pprint/pprint samples)
  (let [sum        (pipeline/total samples)
        ;; _ (clojure.pprint/pprint sum)
        eval-count (:eval-count sum)
        avg        (pipeline/divide sum eval-count)
        paths      (mapcat metric/metric-paths metrics)
        stats      (reduce
                     (fn [res path]
                       (assoc-in res path (stats-for path batch-size samples opts)))
                     {}
                     paths)

        ;; times         (mapv toolkit/elapsed-time samples)
        ;; tail-quantile (:tail-quantile opts 0.025)
        ;; stats         (stats/bootstrap-bca
        ;;                 (mapv double times)
        ;;                 (juxt
        ;;                   stats/mean
        ;;                   stats/variance
        ;;                   (partial stats/quantile 0.5)
        ;;                   (partial stats/quantile tail-quantile)
        ;;                   (partial stats/quantile (- 1.0 tail-quantile)))
        ;;                 (:bootstrap-size opts 100)
        ;;                 [0.5 tail-quantile (- 1.0 tail-quantile)]
        ;;                 well/well-rng-1024a)

        ;; ks [:mean :variance :median :0.025 :0.975 ]
        ;; stats (zipmap ks stats)
        ;; sqr-batch-size (stats/sqr batch-size)
        ;; scale-1 (fn [[p [l u]]]
        ;;            [(/ p batch-size)
        ;;             [(/ l batch-size) (/ u batch-size)]])
        ;; scale-2 (fn [[p [l u]]]
        ;;            [(/ p sqr-batch-size)
        ;;             [(/ l sqr-batch-size) (/ u sqr-batch-size)]]) ;; TODO FIXME
        ;; scale-fns {:mean scale-1
        ;;            :variance scale-2
        ;;            :median scale-1
        ;;            :0.025 scale-1
        ;;            :0.975 scale-1}
        ]

    (cond->
        {:eval-count  eval-count
         :avg         avg
         :stats       stats
         ;; (zipmap ks
         ;;                      (mapv
         ;;                        (fn [k]
         ;;                          ((scale-fns k) (k stats)))
         ;;                        ks))
         :num-samples (count samples)
         :batch-size batch-size
         :metrics metrics}
      return-samples (assoc :samples samples))))


(defn jvm-event-stats
  "Return the stats for JIT compilation and garbage-collection."
  [samples]
  (reduce
    (fn [stats sample]
      (let [update-stat (fn [stat time-ns]
                          (if (pos? time-ns)
                            (-> stat
                               (update :total-time-ns + time-ns)
                               (update :sample-count inc))
                            stat))
            compilation (-> sample :compilation :compilation-time)
            gc          (-> sample :garbage-collector :total :time)]
        (-> stats
           (update :compilation update-stat compilation)
           (update :garbage-collection update-stat gc))))
    {:compilation        {:total-time-ns 0
                          :sample-count  0}
     :garbage-collection {:total-time-ns 0
                          :sample-count  0}}
    samples))
