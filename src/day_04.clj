(ns day-04
  (:require [clojure.string :as str]))

(defn line->pairs
  "Convert a line into a tuple of maps with start and end ranges"
  [line]
  (for [elf  (str/split line #",")
        :let [[start end] (str/split elf #"-")]]
    {:start (Integer/parseInt start)
     :end   (Integer/parseInt end)}))

(defn fully-contained?
  "Return true if the start and end of `a` is fully contained by `b`"
  [a b]
  (and
    (<= (:start b) (:start a))
    (<= (:end a) (:end b))))

(defn overlaps?
  "Return true if `a` and `b` overlap at all"
  [a b]
  (let [[min-seg max-seg] (if (< (:start a) (:start b))
                            [a b]
                            [b a])]
    (not (< (:end min-seg) (:start max-seg)))))

(comment
  ;; Part one
  (->> (slurp "resources/day_04.txt")
       (str/split-lines)
       (map line->pairs)
       (filter #(or (apply fully-contained? %)
                    (apply fully-contained? (reverse %))))
       count)

  ;; Part two
  (->> (slurp "resources/day_04.txt")
       (str/split-lines)
       (map line->pairs)
       (filter #(apply overlaps? %))
       count)
  )
