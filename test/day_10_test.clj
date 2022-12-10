(ns day-10-test
  (:require [day-10 :as sut]
            [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]))

(defonce sample-input
  (slurp "resources/day_10_sample.txt"))

(deftest line->instruction-test
  (is (some? (map sut/line->instruction (str/split-lines sample-input)))))

(deftest run-cylce-test
  (is (= [1 1 4 4 -1]
         (->> (iterate sut/run-cycle {:x 1 :cycle 0 :instructions [[0 [:noop]] [1 [:addx 3]] [1 [:addx -5]]] })
              (rest)
              (take 5)
              (map :x)))))

(deftest solve-part-one-test
  (is (= 13140 (sut/solve-part-one sample-input))))

(deftest cycle->crt-coordinate
  (is (= [0 0] (sut/cycle->crt-coordinate 1)))
  (is (= [0 1] (sut/cycle->crt-coordinate 2)))
  (is (= [1 0] (sut/cycle->crt-coordinate 41))))

(deftest solve-part-two-test
  (is (= (str/trim (slurp "resources/day_10_crt.txt"))
         (sut/solve-part-two sample-input))))
