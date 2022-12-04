(ns day-04-test
  (:require [day-04 :as sut]
            [clojure.test :refer [deftest is are]]))

(deftest line->pairs-test
  (is (= [{:start 2 :end 4}
          {:start 6 :end 8}]
         (sut/line->pairs "2-4,6-8"))))

(deftest fully-contained?-test
  (are [a b] (sut/fully-contained? a b)
    {:start 3 :end 7} {:start 2 :end 8}))

(deftest overlaps?-test
  (are [a b] (sut/overlaps? a b)
    {:start 5 :end 7} {:start 7 :end 9}
    {:start 2 :end 8} {:start 3 :end 7}
    )
  )
