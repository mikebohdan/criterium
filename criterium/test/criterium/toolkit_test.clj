(ns criterium.toolkit-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check
             [clojure-test :refer [defspec]]
             [generators :as gen]
             [properties :as prop]]
            [criterium
             [measured :as measured]
             [pipeline :as pipeline]
             [toolkit :as toolkit]]))


(defspec execution-time-from-batch-returns-pos-long
  (prop/for-all [t gen/pos-int
                 n (gen/fmap inc gen/nat)]
    (pos? (toolkit/execution-time-from-batch t n))))
