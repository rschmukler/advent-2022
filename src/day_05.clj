(ns day-05
  (:require [clojure.string :as str]))

(defn parse-input
  "Parse the given input-text into a map of the state and the
  moves"
  [input-text]
  (let [[state-text moves-text] (->> (str/split-lines input-text)
                                     (split-with #(not= "" %)))
        moves-text              (rest moves-text)
        stack-ixs
        (->> state-text
             last
             (map-indexed (fn [ix c]
                            (when (not= \  c)
                              [(Integer/parseInt (str c)) ix])))
             (remove nil?)
             (into {}))]
    {:state
     (->> (for [line       (reverse (butlast state-text))
                [stack ix] stack-ixs
                :let       [container (try (subs line ix (inc ix))
                                           (catch StringIndexOutOfBoundsException _
                                             nil))]
                :when      (and container
                                (not= " " container))]
            [stack container])
          (reduce
            (fn [acc [stack container]]
              (update acc stack (fnil conj []) container))
            {}))
     :moves
     (for [line moves-text
           :let [[qty from to] (->> (re-seq #"\d+" line)
                                    (map #(Integer/parseInt %)))]]
       {:qty qty :from from :to to})}))


(defn apply-move-part-1
  "Apply the given move to `state` and return the new state"
  [state {:keys [qty from to] :as move}]
  (if (zero? qty)
    state
    (apply-move-part-1
      (let [container (-> state (get from) peek)]
        (-> state
            (update from pop)
            (update to (fnil conj []) container)))
      (update move :qty dec))))

(defn apply-move-part-2
  "Apply the given move to `state` and return the new state"
  [state {:keys [qty from to]}]
  (let [from-stack (get state from)
        end-ix     (- (count from-stack) qty)
        containers (subvec from-stack end-ix)]
    (-> state
        (assoc from (subvec from-stack 0 end-ix))
        (update to into containers))))

(defn solve
  [apply-move-f input-text]
  (let [{:keys [moves state]} (parse-input input-text)
        end-state             (reduce apply-move-f state moves)]
    (apply str
           (for [k (sort (keys end-state))]
             (-> end-state (get k) peek)))))

(comment
  (->> (slurp "resources/day_05.txt")
       (solve apply-move-part-1))

  (->> (slurp "resources/day_05.txt")
       (solve apply-move-part-2))
  )
