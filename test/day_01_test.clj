(ns day-01-test
  (:require [day-01 :as sut]
            [clojure.test :refer [deftest testing is]]))

(def sample-input
  "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(deftest parse-input-test
  (is (= [[1000 2000 3000]
          [4000]
          [5000 6000]
          [7000 8000 9000]
          [10000]]
         (sut/parse-input sample-input))))

(deftest find-max-calories-test
  (is (= 24000
         (-> sample-input
             sut/parse-input
             sut/find-max-calories))))
