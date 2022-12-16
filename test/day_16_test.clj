(ns day-16-test
  (:require [day-16 :as sut]
            [clojure.test :refer [deftest testing is]]))

(defonce sample-input
  (slurp "resources/day_16_sample.txt"))

(def sample-valves
  (sut/input->valves sample-input))

(deftest line->valve-test
  (is (= {:valve "AA" :flow-rate 0 :leads-to ["DD" "II" "BB"] :open false}
         (sut/line->valve "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"))))


(deftest expected-values-test
  (is (= ["DD" 1651]
         (-> sample-valves
             (sut/expected-values "AA" 30)
             first))))
