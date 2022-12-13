(ns day-13-test
  (:require [day-13 :as sut]
            [clojure.test :refer [deftest is are]]))

(defonce sample-input
  (slurp "resources/day_13_sample.txt"))

(deftest parse-input-test
  (is (= 8 (count (sut/parse-input sample-input)))))

(deftest packet-comparator-test
  (are [expected a b] (= expected (sut/packet-comparator a b))
    -1 [1 1 3 1 1]                 [1 1 5 1 1]
    -1 [[1] [2 3 4]]               [[1] 4]
    1  [9]                         [[8 7 6]]
    -1 [[4 4] 4 4]                 [[4 4] 4 4 4]
    1  [7 7 7 7]                   [7 7 7]
    -1 []                          [3]
    1  [[[]]]                      [[]]
    1  [1 [2 [3 [4 [5 6 7]]]] 8 9] [1 [2 [3 [4 [5 6 0]]]] 8 9]))

(deftest solve-part-one-test
  (is (= 13 (sut/solve-part-one sample-input))))

(deftest solve-part-two-test
  (is (= 140 (sut/solve-part-two sample-input))))
