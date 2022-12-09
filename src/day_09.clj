(ns day-09
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(defn dist-x
  "Retunr the distinance between `a` and `b` on the x axis"
  [[a-x _] [b-x _]]
  (- b-x a-x))

(defn dist-y
  "Retunr the distinance between `a` and `b` on the y axis"
  [[_ a-y] [_ b-y]]
  (- b-y a-y))


(defn step-head
  "Return the new head position give the provided `move`"
  {:arglists '([head move])}
  [[x y] [direction _qty]]
  (case direction
    :right [(inc x) y]
    :left  [(dec x) y]
    :up    [x (inc y)]
    :down  [x (dec y)]))

(defn step-knot
  "Return the new knot position given the provided `head` and `tail`"
  [head knot]
  (when (and head knot)
    (let [x-dist (dist-x knot head)
          y-dist (dist-y knot head)]
      (-> knot
          (update 0 + (match [x-dist y-dist]
                        [-2 _]          -1
                        [2  _]           1
                        [1  (:or 2 -2)]  1
                        [-1 (:or 2 -2)] -1
                        [_ _]            0))
          (update 1 + (match [x-dist y-dist]
                        [_ -2]          -1
                        [_  2]           1
                        [(:or 2 -2) 1]   1
                        [(:or 2 -2) -1] -1
                        [_]              0))))))

(defn rope-positions
  "Given a `rope` and a `move`, return a list of all rope positions while
  the move is executing."
  [rope [direction qty :as move]]
  (if (zero? qty)
    nil
    (let [new-rope
          (->> rope
               (reduce
                 (fn build-new-rope [new-rope knot]
                   (if (empty? new-rope)
                     [(step-head knot move)]
                     (conj new-rope (step-knot (peek new-rope) knot))))
                 []))]
      (lazy-seq
        (cons
          new-rope
          (rope-positions new-rope [direction (dec qty)]))))))

(defn line->move
  "Parse a line to a move"
  [line]
  (let [[_ dir qty] (re-find #"(R|U|L|D) (\d+)" line)]
    [(case dir
       "R" :right
       "L" :left
       "U" :up
       "D" :down)
     (Integer/parseInt qty)]))

(defn solve-problem
  [input-text num-knots]
  (->> (loop [result nil
              moves  (map line->move (str/split-lines input-text))
              rope   (vec (repeat num-knots [0 0]))]
         (if (empty? moves)
           result
           (let [positions (rope-positions rope (first moves))]
             (recur (concat result positions) (rest moves) (last positions)))))
       (map last)
       (set)
       (count)))


(comment
  (solve-problem (slurp "resources/day_09.txt") 2)
  (solve-problem (slurp "resources/day_09.txt") 10)
  )
