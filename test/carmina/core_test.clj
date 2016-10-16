(ns carmina.core-test
  (:require [clojure.test :refer :all]
            [carmina.core :refer :all]))

(deftest roll-change-test
  (testing "Do the correct bits return true?"
    (is (= (into [] '(true true true)) (into [] (roll-change 0 3))))
    (is (= (into [] '(true false false)) (into [] (roll-change 1 3))))
    (is (= (into [] '(true true false)) (into [] (roll-change 2 3))))
    (is (= (into [] '(true false false)) (into [] (roll-change 3 3))))
    (is (= (into [] '(true true true)) (into [] (roll-change 4 3))))
    (is (= (into [] '(true false false)) (into [] (roll-change 5 3))))
    (is (= (into [] '(true true false)) (into [] (roll-change 6 3))))
    (is (= (into [] '(true false false)) (into [] (roll-change 7 3))))
    (is (= (into [] '(true true true)) (into [] (roll-change 8 3))))
    (is (= (into [] '(true false false)) (into [] (roll-change 9 3))))
    ))


(deftest one-f-noise-test
  (testing "Tests for one-f noise generators."
    (is (= 8 (count (one-f-noise 3 6))))
    (is (= 4 (count (one-f-noise 2 6))))
    ))
(run-tests 'carmina.core-test)

