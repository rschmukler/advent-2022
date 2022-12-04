(ns day-03
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn line->rucksack
  "Parse a line of input text into a rucksack"
  [line]
  (let [pivot (/ (count line) 2)]
    [(set (subs line 0 pivot))
     (set (subs line pivot))]))

(defn shared-inventory
  "Return the set of items that the rucksack has in both the front and rear compartment"
  [rucksack]
  (set/intersection (first rucksack) (second rucksack)))

(defn char->score
  "Return the score for the given character"
  [c]
  (let [base-val (int c)]
    (if (>= base-val 97)
      (- base-val 96)
      (- base-val 38))))


(comment
  ;;Part one
  (->> (slurp "resources/day_03.txt")
       (str/split-lines)
       (mapcat (comp shared-inventory line->rucksack))
       (map char->score)
       (reduce + 0))

  ;; Part two
  (->> (slurp "resources/day_03.txt")
       (str/split-lines)
       (map line->rucksack)
       (partition 3 3 nil)
       (map (fn [group]
              (->> group
                   (map #(set/union (first %) (second %)))
                   (apply set/intersection)
                   first)))
       (map char->score)
       (reduce + 0))
  )
