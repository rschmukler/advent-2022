(ns day-02
  (:require [clojure.string :as str]))

(defn parse-input
  "Parse the provided string into input tuples of encrypted moves"
  [text]
  (->> text
       (str/split-lines)
       (map #(vec (str/split % #" ")))))

(defn round-result
  "Given a tuple of their move and my move (as keywords) return whether the round is a win,
  loss, or draw."
  [[their-move my-move :as moves]]
  (cond
    (= their-move my-move) :draw
    (some?
      (#{[:rock :paper]
         [:paper :scissors]
         [:scissors :rock]}
        moves))            :win
    :else                  :lose))

(defn round-score
  "Compute the score of the round. Takes a `choose-move-f` which will be given an encrypted move and
  is expected to return a vector of the opponents move and our move."
  [choose-move-f encrypted-moves]
  (let [[_their-move my-move :as moves] (choose-move-f encrypted-moves)
        result->score                   {:win  6
                                         :draw 3
                                         :lose 0}
        shape->score                    {:rock     1
                                         :paper    2
                                         :scissors 3}]
    (+ (shape->score my-move)
       (result->score (round-result moves)))))

(defn part-one-choose-move-f
  "Choose-move-f for part one."
  [encrypted-moves]
  (mapv
    {"A" :rock
     "B" :paper
     "C" :scissors
     "X" :rock
     "Y" :paper
     "Z" :scissors} encrypted-moves))

(defn part-two-choose-move-f
  "Choose-move-f for part two."
  [[their-move target-output]]
  (let [their-move    (case their-move
                        "A" :rock
                        "B" :paper
                        "C" :scissors)
        target-output (case target-output
                        "X" :lose
                        "Y" :draw
                        "Z" :win)
        ->win         {:rock     :paper
                       :scissors :rock
                       :paper    :scissors}
        ->lose        (into {} (map (juxt val key)) ->win)]
    [their-move
     (case target-output
       :draw their-move
       :win  (->win their-move)
       :lose (->lose their-move))]))

(comment
  ;; Part 1
  (->> (slurp "resources/day_02.txt")
       (parse-input)
       (map (partial round-score part-one-choose-move-f))
       (reduce + 0))

  ;; Part 2
  (->> (slurp "resources/day_02.txt")
       (parse-input)
       (map (partial round-score part-two-choose-move-f))
       (reduce + 0))
  )
