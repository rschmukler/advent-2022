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

(defn shortest-path-dj
  "Dijkstra's cost algo"
  [m starts end]
  (loop [queue   (into clojure.lang.PersistentQueue/EMPTY starts)
         state   (into {} (map (juxt identity (constantly {:cost 0}))) starts)
         visited #{}]
    (let [node (peek queue)]
      (if (= end node)
        (->> (iterate (comp :via state) node)
             (take-while some?)
             (reverse)
             (rest))
        (let [neighbors (->> (possible-moves m node)
                             (remove
                               (some-fn (set queue)
                                        visited)))]
          (recur
            (reduce conj (pop queue) neighbors)
            (reduce
              (fn [state neighbor]
                (let [new-cost (inc (:cost (state node)))]
                  (if (< new-cost (:cost (state neighbor) ##Inf))
                    (assoc state neighbor {:cost new-cost :via node})
                    state)))
              state
              neighbors)
            (conj visited node)))))))

(defn pythagoras-dist
  "Return the pythagoras distance between two points"
  [[ya xa] [yb xb]]
  (let [y-dist (abs (- ya yb))
        x-dist (abs (- xa xb))]
    (Math/sqrt (+ (* y-dist y-dist) (* x-dist x-dist)))))

(defn shortest-path-a*
  "Shortest path via a*"
  [m start end]
  (let [f-cost (fn f-cost [node]
                 (let [g-cost (pythagoras-dist node start)
                       h-cost (pythagoras-dist node end)]
                   (+ g-cost h-cost)))]
    (loop [queue   #{start}
           visited #{}
           state   (into {} [start {:cost (f-cost start)}])]
      (let [node (first (sort-by (comp :cost state) queue))]
        (cond
          (nil? node)  nil
          (= end node) (->> (iterate (comp :via state) node)
                            (take-while (every-pred some?
                                                    #(not= start %)))
                            reverse)
          :else
          (let [neighbors (possible-moves m node)
                state
                (reduce
                  (fn keep-lowest-f-cost [state neighbor]
                    (let [f-cost        (f-cost neighbor)
                          existing-cost (get-in state [neighbor :cost] ##Inf)]
                      (if (< f-cost existing-cost)
                        (assoc state neighbor
                               {:cost f-cost :via node})
                        state)))
                  state
                  neighbors)]
            (recur
              (into (disj queue node) (remove visited) neighbors)
              (conj visited node)
              state)))))))

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
    (count (shortest-path-a* m start end))))

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
    (count (shortest-path-dj m starts end))))


(comment
  (solve-part-one (slurp "resources/day_12.txt"))
  (solve-part-two (slurp "resources/day_12.txt"))
  )
