(ns day-13
  (:require [clojure.string :as str]))


(defn parse-input
  "Parse the puzze input"
  [input]
  (->> (str/split-lines input)
       (partition-by #{""})
       (remove #{'("")})
       (mapv #(map read-string %))))

(defn packet-comparator
  "Custom comparator function"
  [a b]
  (cond
    (and (int? a) (int? b)) (compare a b)
    (and (vector? a) (vector? b))
    (loop [[a & as] a
           [b & bs] b]
      (cond
        (and (nil? a) (nil? b)) 0
        (nil? a)                -1
        (nil? b)                1
        :else
        (let [result (packet-comparator a b)]
          (if (zero? result)
            (recur as bs)
            result))))
    (vector? a)             (recur a [b])
    (vector? b)             (recur [a] b)))

(defn solve-part-one
  [input]
  (->> (parse-input input)
       (map-indexed vector)
       (filter #(= -1 (apply packet-comparator (second %))))
       (map (comp inc first))
       (apply +)))

(defn solve-part-two
  [input]
  (let [divider-packets #{[[2]] [[6]]}]
    (->> (parse-input input)
         (mapcat identity)
         (concat divider-packets)
         (sort-by identity packet-comparator)
         (map-indexed vector)
         (filter (comp divider-packets second))
         (map (comp inc first))
         (apply *))))

(comment
  (solve-part-one (slurp "resources/day_13.txt"))
  (solve-part-two (slurp "resources/day_13.txt"))
  )
