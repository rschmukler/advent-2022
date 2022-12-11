(ns day-11-test
  (:require  [clojure.test :refer [deftest is are]]
             [clojure.core.match :refer [match]]
             [day-11 :as sut]
             [wing.core :as w]))

(defonce sample-input
  (slurp "resources/day_11_sample.txt"))

(def sample-monkeys
  (sut/input->monkeys sample-input))

(deftest input->monkeys-test
  (let [monkey (sample-monkeys 0)]
    (is (match [monkey]
          [{:id        0
            :items     [79 98]
            :operation [* :old 19]
            :test      [23 2 3]}] true
          [_] false))))

(deftest process-monkey-turn-test
  (let [new-monkeys (sut/process-monkey-turn sut/part-one-worry-f sample-monkeys 0)]
    (is (= [] (:items (new-monkeys 0))))
    (is (= [74 500 620] (:items (new-monkeys 3))))))

(deftest process-round-test
  (let [monkeys (sut/process-round sut/part-one-worry-f sample-monkeys)]
    (are [expected-inv id] (= expected-inv (:items (monkeys id)))
      [20 23 27 26]              0
      [2080 25 167 207 401 1046] 1
      []                         2
      []                         3)))

(deftest inspection-counts-test
  (is (= {0 101
          1 95
          2 7
          3 105}
         (sut/inspection-counts sut/part-one-worry-f 20 sample-monkeys)))
  (is (= {0 2
          1 4
          2 3
          3 6}
         (sut/inspection-counts (sut/build-part-two-worry-f
                                  sample-monkeys) 1 sample-monkeys)))
  (is (= {0 99
          1 97
          2 8
          3 103}
         (sut/inspection-counts (sut/build-part-two-worry-f sample-monkeys) 20 sample-monkeys))))

(deftest solve-part-one-test
  (is (= 10605 (sut/solve-part-one sample-input))))

(deftest solve-part-two-test
  (is (= 2713310158
         (sut/solve-part-two sample-input))))
