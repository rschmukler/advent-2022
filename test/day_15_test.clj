(ns day-15-test
  (:require [day-15 :as sut]
            [clojure.test :refer [deftest is]]))

(defonce sample-input
  (slurp "resources/day_15_sample.txt"))

(def sensors
  (sut/input->sensors sample-input))


(deftest line->sensor-test
  (is (= {:position       [2 18]
          :closest-beacon [-2 15]}
         (sut/line->sensor "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"))))

(deftest manhattan-distance-test
  (is (= 1 (sut/manhattan-distance [0 1] [0 0])))
  (is (= 2 (sut/manhattan-distance [0 0] [1 1]))))

(deftest consolidate-ranges-test
  (is (= [[-2 25]]
         (sut/consolidate-ranges [[-2 3] [2 15] [14 19] [16 25]])))
  (is (= [[-2 5] [14 25]]
         (sut/consolidate-ranges [[-2 5] [2 4] [14 19] [16 25]])))
  (is (= [[-2 25] [27 30]]
         (sut/consolidate-ranges [[-2 3] [2 15] [14 19] [16 25] [27 30]]))))

(deftest count-of-places-beacon-cant-be-test
  (is (= 26 (sut/count-of-places-beacon-cant-be sensors 10))))

(deftest find-tuning-frequency
  (is (= 56000011 (sut/find-tuning-frequency sensors 20))))
