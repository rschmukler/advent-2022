(ns day-08
  (:require [clojure.string :as str]))


(defn input->grid
  "Parse the given input into a grid"
  [input-text]
  (->> (for [[y line] (map-indexed vector (str/split-lines input-text))
             [x c]    (map-indexed vector line)]
         [[y x] (Integer/parseInt (str c))])
       (into {})))

(defn max-x
  [grid]
  (->> grid
       (keys)
       (map second)
       (apply max)))

(defn max-y
  [grid]
  (->> grid
       (keys)
       (map first)
       (apply max)))

(defn project
  "Return a sequence of grid points"
  [grid [origin-y origin-x] dir]
  (case dir
    :left (reverse (for [x (range 0 origin-x)] [origin-y x]))
    :right (for [x (range (inc origin-x) (inc (max-x grid)))] [origin-y x])
    :up (reverse (for [y (range 0 origin-y)] [y origin-x]))
    :down (for [y (range (inc origin-y) (inc (max-y grid)))] [y origin-x])))

(defn visible-from
  "Return the directions that the provided tree is visible from"
  [grid tree]
  (let [tree-height (get grid tree)]
    (->> [(when (every? #(> tree-height %) (map grid (project grid tree :left)))
            :left)
          (when (every? #(> tree-height %) (map grid (project grid tree :up)))
            :up)
          (when (every? #(> tree-height %) (map grid (project grid tree :right)))
            :right)
          (when (every? #(> tree-height %) (map grid (project grid tree :down)))
            :down)]
         (remove nil?)
         set)))

(defn visible-trees-count
  "Return the number of visible trees in the grid"
  [grid]
  (->> grid
       (keys)
       (filter #(seq (visible-from grid %)))
       count))

(defn visible-trees-from
  "Return all trees that the given ``origin` can see"
  [grid origin]
  (let [origin-height (grid origin)]
    (loop [result {:left #{} :right #{} :up #{} :down #{}}
           dirs   [:up :left :right :down]
           dir    nil
           coords nil]
      (let [tree        (first coords)
            tree-height (grid tree)]
        (cond (and (empty? coords) (empty? dirs)) result
              (empty? coords)
              (recur result (rest dirs) (first dirs) (project grid origin (first dirs)))
              (< tree-height origin-height)
              (recur (update result dir conj tree) dirs dir (rest coords))
              (>= tree-height origin-height)
              (recur (update result dir conj tree) dirs dir nil)
              :else
              (recur result dirs dir nil))))))

(defn scenic-score
  [grid tree]
  (->> (visible-trees-from grid tree)
       (vals)
       (map count)
       (apply *)))

(defn find-max-scenic-score
  [grid]
  (->> (keys grid)
       (map #(scenic-score grid %))
       (apply max)))
(comment
  ;; Part one
  (-> (slurp "resources/day_08.txt")
      (input->grid)
      (visible-trees-count))

  ;; Part two
  (let [grid (input->grid (slurp "resources/day_08.txt"))]
    (->> (keys grid)
         (map (juxt identity #(scenic-score grid %)))
         (sort-by first >)))
  )
