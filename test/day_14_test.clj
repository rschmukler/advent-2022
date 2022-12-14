(ns day-14-test
  (:require [day-14 :as sut]
            [clojure.test :refer [deftest testing is]]))

(def sample-input
  "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")

(def sample-cave
  (sut/input->cave sample-input))

(deftest input->cave-test
  (is (sample-cave [494 9]))
  (is (sample-cave [495 9]))
  (is (sample-cave [502 9]))
  (is (not (sample-cave [503 9]))))

(deftest simulate-next-sand-test
  (let [new-cave (sut/place-sand sample-cave)]
    (is (sut/sand? new-cave [500 8]))
    (is (sut/sand? (sut/place-sand new-cave) [499 8]))))

(deftest placable-sand-count
  (is (= 24 (sut/placable-sand-count sample-cave)))
  (is (= 93 (sut/placable-sand-count (sut/add-floor sample-cave)))))
