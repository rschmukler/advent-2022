(ns day-08-test
  (:require [day-08 :as sut]
            [clojure.test :refer [deftest are is]]))

(def sample-input
  "30373\n25512\n65332\n33549\n35390")

(deftest input->grid-test
  (is (= {[0 0] 1
          [0 1] 2
          [1 0] 3
          [1 1] 4}
         (sut/input->grid "12\n34"))))

(deftest visible-from-test
  (is (= #{:left :up}
         (sut/visible-from
           (sut/input->grid sample-input)
           [1 1])))
  (is (= #{:up :right}
         (sut/visible-from
           (sut/input->grid sample-input)
           [1 2]))))

(deftest visible-trees-count-test
  (is (= 21 (-> sample-input sut/input->grid sut/visible-trees-count))))

(deftest visible-trees-from-test
  (is (= {:up    #{[0 2]}
          :left  #{[1 1]}
          :right #{[1 3] [1 4]}
          :down  #{[2 2] [3 2]}}
         (sut/visible-trees-from
           (sut/input->grid sample-input)
           [1 2]))))

(deftest scenic-score-test
  (are [expected tree] (= expected (sut/scenic-score
                                     (sut/input->grid sample-input)
                                     tree))
    4 [1 2]
    8 [3 2]))

(deftest find-max-scenic-score-test
  (is (= 8
         (sut/find-max-scenic-score (sut/input->grid sample-input)))))
