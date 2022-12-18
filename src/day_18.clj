(ns day-18
  (:require [clojure.string :as str]))

(defn line->coordinate
  "Parse a line into a coordinate triplet tuple"
  [line]
  (map read-string (str/split line #",")))

(def ->x first)
(def ->y second)
(def ->z #(nth % 2))

(defn compute-stats
  "Compute stats and associate it as meta data on the provided coordinates"
  [coords]
  (with-meta coords
    {:max-x (apply max (map ->x coords))
     :min-x (apply min (map ->x coords))
     :max-y (apply max (map ->y coords))
     :min-y (apply min (map ->y coords))
     :max-z (apply max (map ->z coords))
     :min-z (apply min (map ->z coords))}))

(defn input->coordinates
  "Parses the sample input into all coordinates"
  [input]
  (-> (into #{} (map line->coordinate) (str/split-lines input))
      (compute-stats)))

(defn adjacent-sides
  "Return the adjacent sides for the provided `pixel`"
  [[x y z]]
  #{[(inc x) y z]
    [(dec x) y z]
    [x (inc y) z]
    [x (dec y) z]
    [x y (inc z)]
    [x y (dec z)]})

(defn surface-area
  "Return the exposed surface area of the provided `coords`"
  [coords]
  (->> coords
       (mapcat adjacent-sides)
       (remove coords)
       (count)))

(defn out-of-bounds?
  "Return whether the provided position is out of bounds"
  [coords [x y z]]
  (let [{:keys [min-x min-y min-z
                max-x max-y max-z]} (meta coords)]
    (or (not (<= min-x x max-x))
        (not (<= min-y y max-y))
        (not (<= min-z z max-z)))))

(defn contained?
  "Return whether the given position is contained by coords"
  [coords pos]
  (loop [q       (into clojure.lang.PersistentQueue/EMPTY [pos])
         visited #{}]
    (let [pos (peek q)]
      (cond
        (nil? pos)                  true
        (out-of-bounds? coords pos) false
        :else
        (let [neighbors (->> (adjacent-sides pos)
                             (remove (some-fn visited
                                              coords
                                              (set q))))]
          (recur (into (pop q) neighbors) (conj visited pos)))))))


(defn find-air-pockets
  "Finds pockets of air within the provided coordinates. Return them as sets
  of coordinates"
  [coords]
  (let [{:keys [min-x max-x min-y max-y min-z max-z]} (meta coords)
        contained?                                    (memoize contained?)]
    (->> (for [x (range min-x (inc max-x))
               y (range min-y (inc max-y))
               z (range min-z (inc max-z))]
           [x y z])
         (remove coords)
         (filter #(contained? coords %))
         (set))))


(defn surface-area-without-air-pockets
  [coords]
  (let [air-pockets (find-air-pockets coords)]
    (surface-area (into coords air-pockets))))

(comment
  (-> (slurp "resources/day_18.txt")
      (input->coordinates)
      (surface-area))

  (-> (slurp "resources/day_18.txt")
      (input->coordinates)
      (surface-area-without-air-pockets))
  )
