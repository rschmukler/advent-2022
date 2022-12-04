(ns day-03-test
  (:require [day-03 :as sut]
            [clojure.test :refer [deftest testing is]]))

(def sample-input
  "vJrwpWtwJgWrhcsFMMfFFhFp")

(deftest shared-inventory-test
  (is (= #{\p}
         (-> sample-input
             sut/line->rucksack
             sut/shared-inventory))))

(deftest char->score-test
  (is (= 1 (sut/char->score \a)))
  (is (= 27 (sut/char->score \A))))
