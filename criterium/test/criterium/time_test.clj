(ns criterium.time-test
  (:require [criterium
             [time :as time]]
            [clojure.test :refer [deftest is]]))

(deftest time-test
  (is time/last-time*))
