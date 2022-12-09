(ns day-09-test
  (:require [day-09 :as sut]
            [clojure.test :refer [deftest testing is]]))

(def sample-input-part-one
  "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2")

(def sample-input-part-two
  "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20")


(deftest rope-positions-test
  (is (= [[[1 0] [0 0]]
          [[2 0] [1 0]]
          [[3 0] [2 0]]
          [[4 0] [3 0]]]
         (sut/rope-positions [[0 0] [0 0]] [:right 4]))
      (= [[[4 1] [3 0]]
          [[4 2] [4 1]]
          [[4 3] [4 2]]
          [[4 4] [4 3]]]
         (sut/rope-positions [[4 0] [3 0]] [:up 4]))))

(deftest solve-problem-test
  (is (= 13 (sut/solve-problem sample-input-part-one 2)))
  (is (= 36 (sut/solve-problem sample-input-part-two 10))))
