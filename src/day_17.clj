(ns day-17
  (:require [clojure.string :as str]
            [wing.core :as w]))

(def rock-cycle
  "An infinite cycle of shapes"
  (cycle (map str "-+L|#")))

(defn rock->coordinates
  "Take a rock and return all of its coordinates"
  [{:keys [shape] [x y] :position}]
  (->> (case shape
         "-" '([0 0] [1 0] [2 0] [3 0])
         "+" '([0 1] [1 0] [1 1] [1 2] [2 1])
         "L" '([0 0] [1 0] [2 0] [2 1] [2 2])
         "|" '([0 0] [0 1] [0 2] [0 3])
         "#" '([0 0] [1 0] [0 1] [1 1]))
       (map #(vector (+ x (first %1)) (+ y (second %1))))))


(defn collision-left?
  "Return whether the left edge of rock is coliding with anything"
  [m {:keys [shape] [x y :as pos] :position}]
  (case shape
    "-" (or (m pos) (neg? x))
    "+" (or (m [x (inc y)])
            (neg? x))))

(defn in-bounds?
  "Return whether the provided position is in-bounds"
  [[x y]]
  (and (<= 0 x 6) (nat-int? y)))

(defn attempt-move
  "Takes the given rock and attempts to move it in the provided directioin.
  Returns the new rock if the position cam be updated, or the same rock if it can't"
  [{[x y] :position :as rock} m move]
  (let [new-rock (assoc rock :position (case move
                                         :left  [(dec x) y]
                                         :right [(inc x) y]
                                         :down  [x (dec y)]))
        coords   (rock->coordinates new-rock)]
    (if (and (in-bounds? (:position new-rock))
             (every? in-bounds? coords)
             (not-any? m coords))
      new-rock
      rock)))

(defn trim-to-top
  "Take a set of coordinates and only return the highest y-value
  for each coordinate in the x plain"
  [coords]
  (let [y->coords (w/group-by second coords)]
    (loop [y       (apply max (keys y->coords))
           seen-xs #{}
           result  #{}]
      (if (or (= 7 (count seen-xs)) (neg-int? y))
        result
        (let [coords-at-y (y->coords y)
              new-xs      (map first coords-at-y)]
          (recur
            (dec y)
            (into seen-xs new-xs)
            (into result coords-at-y)))))))

(defn normalize-top
  "Take coordinates in `coords` and return them normalized to relative heights"
  [coords]
  (let [min-y (apply min (map second coords))]
    (set
      (for [[x y] coords]
        [x (- y (dec min-y))]))))

(defn place-rock
  "Simulate one rock falling and return the new state"
  [{:keys [m stream rock-shapes]}]
  #_(println "A new rock begins falling")
  (loop [rock                 {:shape    (first rock-shapes)
                               :position [2 (+ 4 (apply max -1 (map second m)))]}
         [move & rest-stream] stream]
    (let [moved-rock (attempt-move rock m (case move "<" :left ">" :right))
          new-rock   (attempt-move moved-rock m :down)]
      #_(println
          "Jet of gas pushes rock"
          (case move "<" "left" ">" "right")
          (if (= moved-rock rock)
            "nothing happens"
            " "))
      #_(println
          "Rock falls 1 unit"
          (if (= moved-rock new-rock)
            "causing it to come to rest"
            " "))
      (if (not= new-rock moved-rock)
        (recur new-rock rest-stream)
        {:m           (trim-to-top (into m (rock->coordinates moved-rock)))
         :stream      rest-stream
         :rock-shapes (rest rock-shapes)}))))

(defn rock-placements
  "Return an infinite sequence of rock placements"
  [stream]
  (->> (iterate place-rock {:m           #{}
                            :rock-shapes rock-cycle
                            :stream      (cycle (map str stream))})
       (map :m)))


(defn place-rocks
  "Place `n` rocks using the provided jet stream and return the end map."
  [n stream]
  (->> (rock-placements stream)
       (drop n)
       (first)))

(defn height
  "Return the height of m"
  [m]
  (inc (apply max 0 (map second m))))

(defn find-cycle
  "Given a sequence of items, attempts to find a cycle"
  [items]
  (let [items (vec items)]
    (loop [n 10]
      (cond
        (= n (count items)) nil
        (= (take-last n items)
           (take n (take-last (* 2 n) items)))
        n
        :else
        (recur (inc n))))))

(defn quick-compute-height-at-level
  "Optimized height computation"
  [n stream]
  (let [rocks       (->> (rock-placements stream)
                         (rest)
                         (take 5000))
        height-seq  (map height rocks)
        sample-size (->> rocks
                         (map normalize-top)
                         find-cycle)
        sample-rem  (rem n sample-size)
        sample-quot (quot n sample-size)]
    (+ (* sample-quot
          (->> height-seq
               (partition 2 1)
               (take-last sample-size)
               (map #(- (second %) (first %)))
               (apply +)))
       (-> height-seq
           (nth (dec sample-rem))))))

(defn print-rocks
  "Debug function to print some rocks"
  [rocks]
  (let [min-y (apply min (map second rocks))]
    (loop [y (apply max (map second rocks))]
      (if (< y min-y)
        nil
        (do (println (inc y) "  " (apply str (for [x (range 0 7)]
                                               (if (rocks [x y])
                                                 "#"
                                                 "."))))
            (recur (dec y)))))
    rocks))

(comment
  (->> (slurp "resources/day_17.txt")
       (str/trim)
       (quick-compute-height-at-level 1000000000000)))
