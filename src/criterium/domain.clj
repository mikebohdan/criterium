(ns criterium.domain
  (:require [clojure.spec.alpha :as s]))

(s/def ::elapsed-time-ns nat-int?)
(s/def ::eval-count nat-int?)
(s/def ::count nat-int?)
