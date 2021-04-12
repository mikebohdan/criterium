(ns criterium.ziggurat-test
  (:require
   [clojure.test :refer [is]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.stats :as stats]
   [criterium.test-utils :refer [abs-error]]
   [criterium.ziggurat :as ziggurat]))

(defspec random-normal-zig-test-property 10
  (prop/for-all
      [random-seed gen/int]
    (let [random-source  (java.util.Random. random-seed)
          values         (->> #(.nextDouble random-source)
                              repeatedly
                              ziggurat/random-normal-zig
                              (take 10000)
                              vec)
          mean           (stats/mean values)
          variance       (stats/variance values)
          mean-error     (abs-error mean 0)
          variance-error (abs-error variance 1)
          mean-tol       1e-1
          variance-tol   15e-1]
      (is (< mean-error mean-tol))
      (is (< variance-error variance-tol))
      (and (< mean-error mean-tol)
           (< variance-error variance-tol)))))
