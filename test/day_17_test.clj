(ns day-17-test
  (:require [day-17 :as sut]
            [clojure.test :refer [deftest testing is]]))

(def jet-stream
  ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(deftest place-rock-test
  (is (= #{[2 0] [3 0] [4 0] [5 0]}
         (sut/place-rocks 1 jet-stream)))
  (is (= (into (sut/place-rocks 1 jet-stream)
               #{[3 1]
                 [2 2] [3 2] [4 2]
                 [3 3]})
         (sut/place-rocks 2 jet-stream)))
  (is (= (into (sut/place-rocks 2 jet-stream)
               #{[0 3]
                 [1 3]
                 [2 3]
                 [2 4]
                 [2 5]})
         (sut/place-rocks 3 jet-stream))))

(deftest height-test
  (is (= 3068
         (sut/height (sut/place-rocks 2022 jet-stream)))))

(deftest quick-compute-height-at-level-test
  (is (= 3068
         (sut/quick-compute-height-at-level 2022 jet-stream)))
  (is (= 1514285714288
         (sut/quick-compute-height-at-level
           1000000000000
           jet-stream))))

(comment
  (->> (sut/rock-placements jet-stream)
       (map-indexed vector)
       (rest)
       (keep #(when (zero? (rem (first %) 200))
                (sut/height (second %))))
       (take 22)
       (reverse)
       (partition 2 1)
       (map #(apply - %))))


(defn find-cycle
  [items]
  (loop [offset 1
         s      (rest items)]
    (cond
      (= (take 5 s)
         (take 5 items)) offset
      (> offset 100000)  nil
      :else
      (recur (inc offset) (rest s)))))

(find-cycle
  (cycle "abcdefghijk"))

(comment
  (sut/find-cycle
    (->> (sut/rock-placements jet-stream)
         (rest)
         (map sut/height)
         (partition 2 1)
         (map #(- (second %) (first %)))))

  (->> (sut/rock-placements jet-stream)
       (rest)
       (take 4200)
       (last)
       (sut/height))
  (let [height-diffs (->> (sut/rock-placements jet-stream)
                          (rest)
                          (map sut/height)
                          (partition 2 1)
                          (map #(- (second %) (first %)))
                          (take 50000))
        offset       1
        s            (drop offset height-diffs)]
    [(take 10 height-diffs)
     (take 10 s)])
  )
