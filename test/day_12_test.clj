(ns day-12-test
  (:require  [clojure.test :refer [deftest testing is]]
             [day-12 :as sut]))

(defonce sample-input
  (slurp "resources/day_12_sample.txt"))

(def sample-map
  (sut/input->map sample-input))

(deftest possible-moves-test
  (is (= [[1 0]
          [0 1]]
         (sut/possible-moves sample-map [0 0]))))


(deftest solve-part-one-test
  (is (= 31
         (sut/solve-part-one sample-input)))
  (is (= 29
         (sut/solve-part-two sample-input))))
