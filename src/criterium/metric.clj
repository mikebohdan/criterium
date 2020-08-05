(ns criterium.metric)

(def metric-paths
  {:elapsed-time-ns    [[:elapsed-time-ns]]
   :memory             [[:memory :total :used]
                        [:memory :heap :used]]
   :class-loader       [[:class-loader :loaded-count]
                        [:class-loader :unloaded-count]]
   :compilation-time   [[:compilation-time :compilation-time]]
   :runtime-memory     [[:runtime-memory :max]
                        [:runtime-memory :free]
                        [:runtime-memory :total]]
   :finalization-count [:finalization-count :pending]
   :garbage-collector  [[:garbage-collector :total :time]
                        [:garbage-collector :total :count]]})

(def metric-format
  {[:elapsed-time-ns]                    {:dimension :ns :label "Elapsed Time"}
   [:memory :total :used]                {:dimension :memory
                                          :label     "Total Used Memory"}
   [:memory :heap :used]                 {:dimension :memory
                                          :label     "Total Heap Memory"}
   [:class-loader :loaded-count]         {:dimension :count :label "Classes Loaded"}
   [:class-loader :unloaded-count]       {:dimension :count :label "Classes Unloaded"}
   [:compilation-time :compilation-time] {:dimension :ns :label "Compilation Time"}
   [:runtime-memory :max]                {:dimension :memory
                                          :label     "Max Memory (runtime)"}
   [:runtime-memory :free]               {:dimension :memory
                                          :label     "Free Memory (runtime)"}
   [:runtime-memory :total]              {:dimension :memory
                                          :label     "Total Memory (runtime)"}
   [:finalization-count :pending]        {:dimension :count
                                          :label     "Finalizations Pending"}
   [:garbage-collector :total :time]     {:dimension :ns :label "GC Time"}
   [:garbage-collector :total :count]    {:dimension :count :label "GC count"}})

(defn path-accessor
  [path]
  (reduce comp (reverse path)))
