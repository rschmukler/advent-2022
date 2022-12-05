(ns day-05-test
  (:require [day-05 :as sut]
            [clojure.test :refer [deftest testing is]]))


(defonce sample-input
  (slurp "resources/day_05_sample.txt"))

(deftest parse-input-test
  (is (= {:state {1 ["Z" "N"]
                  2 ["M" "C" "D"]
                  3 ["P"]}
          :moves [{:qty 1 :from 2 :to 1}
                  {:qty 3 :from 1 :to 3}
                  {:qty 2 :from 2 :to 1}
                  {:qty 1 :from 1 :to 2}]}
         (sut/parse-input sample-input))))

(deftest apply-move-part-1-test
  (is (= {1 ["Z" "N" "D"]
          2 ["M" "C"]
          3 ["P"]}
         (sut/apply-move-part-1
           {1 ["Z" "N"]
            2 ["M" "C" "D"]
            3 ["P"]}
           {:qty 1 :from 2 :to 1}))))

(deftest apply-move-part-2-test
  (is (= {1 []
          2 ["M" "C"]
          3 ["P" "Z" "N" "D"]}
         (sut/apply-move-part-2
           {1 ["Z" "N" "D"]
            2 ["M" "C"]
            3 ["P"]}
           {:qty 3 :from 1 :to 3}))))

(deftest solve-test
  (is (= "CMZ" (sut/solve sut/apply-move-part-1 sample-input)))
  (is (= "MCD" (sut/solve sut/apply-move-part-2 sample-input))))
