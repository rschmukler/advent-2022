(ns day-07-test
  (:require  [clojure.test :refer [deftest is are]]
             [day-07 :as sut]))

(defonce sample-input
  (slurp "resources/day_07_sample.txt"))

(deftest line->value-test
  (are [expected input] (= expected (sut/line->value input))
    [:cd "/"]                "$ cd /"
    [:ls]                    "$ ls"
    [:dir "a"]               "dir a"
    [:file "b.txt" 14848514] "14848514 b.txt"))

(deftest input->dirmap-test
  (is (= {"a"
          {"e"     {"i" 584}
           "f"     29116
           "g"     2557
           "h.lst" 62596}
          "b.txt" 14848514
          "c.dat" 8504156
          "d"
          {"j"     4060174
           "d.log" 8033020
           "d.ext" 5626152
           "k"     7214296}}
         (sut/input->dirmap sample-input))))


(deftest ->size-test
  (are [size input] (= size (sut/->size input))
    1 1
    2 {"a" 1 "b" 1}
    3 {"a" 1 "b" {"c" 1 "d" 1}}))

(deftest solve-part-one-test
  (is (= 95437 (sut/solve-part-one sample-input))))

(deftest solv-part-two-test
  (is (= 24933642 (sut/solve-part-two sample-input))))
