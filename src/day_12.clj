(ns day-12
  (:require [clojure.string :as str]))

(defn char->elevation
  "Map the provided charachter into its elevation"
  [c]
  (when c
    (case c
      \S 0
      \E (- (int \z) (int \a))
      (- (int c) (int \a)))))

(defn input->map
  "Parse the provided input into a map"
  [input]
  (->> (for [[y line] (map-indexed vector (str/split-lines input))
             [x c]    (map-indexed vector line)]
         [[y x] c])
       (into {})))

(defn possible-moves
  "Return all possible moves for the provided position"
  [m [y x :as pos]]
  (let [elevation (char->elevation (m pos))]
    (->> [[(inc y) x]
          [(dec y) x]
          [y (inc x)]
          [y (dec x)]]
         (keep #(when-some [new-elevation (char->elevation (m %))]
                  (when (<= (- new-elevation elevation) 1)
                    %))))))

(defn minimum-cost
  "Dijkstra's cost algo"
  [m starts end]
  (loop [queue  (into clojure.lang.PersistentQueue/EMPTY starts)
         ->min-cost (into {} (map (juxt identity (constantly 0))) starts)
         visited #{}]
    (let [node (peek queue)]
      (if (= end node)
        (->min-cost node)
        (let [neighbors (->> (possible-moves m node)
                             (remove
                               (some-fn (set queue)
                                        visited)))]
          (recur
            (reduce conj (pop queue) neighbors)
            (reduce
              #(update %1 %2 (fnil min ##Inf) (inc (->min-cost node)))
              ->min-cost
              neighbors)
            (conj visited node)))))))

(defn solve-part-one
  "Solve part one"
  [input]
  (let [m     (input->map input)
        start (->> m
                   (keys)
                   (filter #(= \S (m %)))
                   first)
        end   (->> m
                   (keys)
                   (filter #(= \E (m %)))
                   first)]
    (minimum-cost m [start] end)))

(defn solve-part-two
  [input]
  (let [m      (input->map input)
        end    (->> m
                    (keys)
                    (filter #(= \E (m %)))
                    first)
        starts (->> m
                    (keys)
                    (filter (comp zero? char->elevation m)))]
    (minimum-cost m starts end)))


(comment
  (possible-moves (input->map
                    (slurp "resources/day_12.txt"))
                  [21 0])
  (solve-part-one (slurp "resources/day_12.txt"))
  (solve-part-two (slurp "resources/day_12.txt"))
  )
