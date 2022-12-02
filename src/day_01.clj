(ns day-01
  (:require [clojure.string :as str]))


(defn parse-input
  "Parse the given input into inventories"
  [s]
  (->> s
       (str/split-lines)
       (partition-by #{""})
       (remove #{'("")})
       (map #(map (fn [num] (Integer/parseInt num)) %))))

(defn sort-by-calories
  [input]
  (->> input
       (map #(reduce + 0 %))
       (sort >)))

(comment
  ;; Part 1
  (->> (slurp "resources/day_01.txt")
       parse-input
       sort-by-calories
       first)

  ;; Part 2
  (->> (slurp "resources/day_01.txt")
       parse-input
       sort-by-calories
       (take 3)
       (apply +)))
