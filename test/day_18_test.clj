(ns day-18-test
  (:require [day-18 :as sut]
            [clojure.test :refer [deftest testing is]]))

(defonce sample-input
  (slurp "resources/day_18_sample.txt"))

(def sample-coords
  (sut/input->coordinates sample-input))

(deftest line->coordinate-test
  (is (= [1 2 3]
         (sut/line->coordinate "1,2,3"))))

(deftest surface-area-test
  (is (= 64
         (sut/surface-area sample-coords))))


(deftest find-air-pockets-test
  (is (= #{[2 2 5]}
         (sut/find-air-pockets sample-coords))))

(deftest surface-area-without-air-pockets-test
  (is (= 58
         (sut/surface-area-without-air-pockets sample-coords))))
