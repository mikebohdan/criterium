(ns criterium.util-test
  (:require [clojure.test :refer [deftest is testing]]
            [criterium.util :as util]))

(def this-ns (ns-name *ns*))

(deftest optional-require-test
  (in-ns this-ns)
  (testing "optional-require on a non-existing namespace"
    (testing "with no alias"
      (util/optional-require '[non-existing])
      (testing "creates a loaded namespace with a stub var"
        (is (ns-resolve 'non-existing 'stub))))
    (testing "with an alias"
      (util/optional-require '[non-existing2 :as ns2])
      (is (ns-resolve 'non-existing2 'stub))
      (testing "creates an alias"
        (is (= (the-ns 'non-existing2) (get (ns-aliases *ns*) 'ns2)))))
    (testing "with refers"
      (util/optional-require '[non-existing3 :refer [abc def]])
      (is (ns-resolve 'non-existing3 'stub))
      (testing "creates vars for each referred symbol"
        (is (ns-resolve 'non-existing3 'abc))
        (is (ns-resolve this-ns 'abc))
        (is (ns-resolve this-ns 'def))))))
