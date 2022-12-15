(ns day-15
  (:require [clojure.string :as str]))


(defn line->sensor
  "Parse a line of text into a sensor"
  [line]
  (let [[x y closest-x closest-y] (map read-string (re-seq #"-?\d+" line))]
    {:position [x y] :closest-beacon [closest-x closest-y]}))

(defn input->sensors
  "Parse puzzle input into a collection of sensors"
  [input]
  (->> (str/split-lines input)
       (map line->sensor)))

(defn x-dist
  "Return the x-distance between two points"
  [[ax _] [bx _]]
  (abs (apply - (sort > [ax bx]))))

(defn y-dist
  "Return the y-distance between two points"
  [[_ ay] [_ by]]
  (abs (apply - (sort > [ay by]))))

(defn manhattan-distance
  "Return the manhattan distance between two points"
  [pt-a pt-b]
  (+ (x-dist pt-a pt-b) (y-dist pt-a pt-b)))

(defn consolidate-ranges
  "Take a sequence of `ranges` (min max tuples) and consolidate them"
  [ranges]
  (loop [[[min-a max-a :as a]
          [min-b max-b :as b]
          & rest-ranges
          :as ranges] (sort-by first ranges)]
    (cond
      (or (nil? a) (nil? b)) ranges
      (>= (inc max-a) min-b) (recur (cons [min-a (max max-b max-a)] rest-ranges))
      :else                  (cons a (consolidate-ranges (cons b rest-ranges))))))

(defn impossible-beacon-ranges
  "Return a set of positions that a beacon can't be on a given line y"
  [y sensors]
  (-> (for [{closest-beacon             :closest-beacon
             [pos-x pos-y :as position] :position}
            sensors
            :let
            [mh-d     (manhattan-distance position closest-beacon)
             y-dist   (apply - (sort > [y pos-y]))
             x-radius (max (- mh-d y-dist) 0)]
            :when (pos? x-radius)
            :let  [blocked-min-x (- pos-x x-radius)
                   blocked-max-x (+ pos-x x-radius)]]
        [blocked-min-x blocked-max-x])
      (consolidate-ranges)))

(defn count-of-places-beacon-cant-be
  "Return the count of places that the beacon cant exist on the given line"
  [sensors y]
  (->> sensors
       (impossible-beacon-ranges y)
       (map #(- (second %1) (first %1)))
       (reduce +)))

(defn find-tuning-frequency
  "Find the tuning frequency for the given sensors and search space size"
  [sensors search-space-size]
  (loop [y 0]
    (when (<= y search-space-size)
      (let [[[_min-x max-x]
             b] (impossible-beacon-ranges y sensors)]
        (if b
          (+ y (* (inc max-x) 4000000))
          (recur (inc y)))))))

(comment
  (-> (slurp "resources/day_15.txt")
      (input->sensors)
      (count-of-places-beacon-cant-be 2000000))
  (future
    (-> (slurp "resources/day_15.txt")
        (input->sensors)
        (find-tuning-frequency 4000000)))
  )
