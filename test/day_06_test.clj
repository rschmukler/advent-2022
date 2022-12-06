(ns day-06-test
  (:require [day-06 :as sut]
            [clojure.test :refer [deftest testing is are]]))

(deftest find-packet-start-ix-test
  (testing "packet size 4"
    (are [expected input] (= expected (sut/find-packet-start-ix input 4))
      5 "bvwbjplbgvbhsrlpgdmjqwftvncz"
      6 "nppdvjthqldpwncqszvftbrmjlhg"))
  (testing "packet size 14"
    (are [expected input] (= expected (sut/find-packet-start-ix input 14))
      19 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
      23 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
