(ns day-14
  (:require [clojure.string :as str]))

(defn input->cave
  "Take the text input and convert it into a cave data structure"
  [input]
  (let [cave
        (into
          {}
          (for [line      (str/split-lines input)
                [[ax ay]
                 [bx by]] (->> (re-seq #"\d+\,\d+" line)
                               (map #(mapv read-string (str/split % #",")))
                               (partition 2 1))
                x         (range (min ax bx) (inc (max ax bx)))
                y         (range (min ay by) (inc (max ay by)))]
            [[x y] "#"]))]
    (with-meta cave {:max-y (apply max (map second (keys cave)))})))

(def sand
  "Constant for the sand character"
  "o")

(def rock
  "Constant for the rock character"
  "#")

(def source-pos
  "Static constant for the source position"
  [500 0])

(defn occupied?
  "Return whether the provided position is occupied"
  [cave pos]
  (some? (cave pos)))

(defn falling-to-abyss?
  "Return whether the given position is within the cave bounds"
  [cave [_x y]]
  (> y (:max-y (meta cave))))

(defn place-sand
  "Take the given cave and place a new piece of sand, returning the
  updated cave. Returns `nil` if no new sand will come to a rest"
  [cave]
  (loop [[sx sy :as sand-pos] source-pos]
    (let [next-move (->> [[sx (inc sy)]
                          [(dec sx) (inc sy)]
                          [(inc sx) (inc sy)]]
                         (remove #(occupied? cave %))
                         (first))]
      (cond
        (occupied? cave source-pos)        nil
        (nil? next-move)                   (assoc cave sand-pos sand)
        (falling-to-abyss? cave next-move) nil
        :else                              (recur next-move)))))

(defn placable-sand-count
  "Solve the puzzle using the provided cave"
  [cave]
  (->> (iterate place-sand cave)
       (rest)
       (take-while some?)
       count))

(defn add-floor
  "Add a floor to the provided cave "
  [cave]
  (let [floor-height (-> cave meta :max-y (+ 2))]
    (reduce
      #(assoc %1 [%2 floor-height] rock)
      (with-meta cave {:max-y floor-height})
      (range -10000 1000))))

(comment
  ;; Part one
  (->> (slurp "resources/day_14.txt")
       (input->cave)
       (placable-sand-count))

  (->> (slurp "resources/day_14.txt")
       (input->cave)
       (add-floor)
       (placable-sand-count))
  )
