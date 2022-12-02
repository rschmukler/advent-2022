(ns day-02-test
  (:require  [clojure.test :refer [deftest is are testing]]
             [day-02 :as sut]))

(def sample-input
  "A Y\nB X\nC Z")

(deftest parse-input-test
  (is (= [["A" "Y"]
          ["B" "X"]
          ["C" "Z"]]
         (sut/parse-input sample-input))))

(deftest round-score-test
  (testing "part 1"
    (are [score moves] (= score (sut/round-score sut/part-one-choose-move-f moves))
      8 ["A" "Y"]
      1 ["B" "X"]
      6 ["C" "Z"])
    (are [score moves] (= score (sut/round-score sut/part-two-choose-move-f moves))
      4 ["A" "Y"]
      1 ["B" "X"]
      7 ["C" "Z"])))
