(ns day-10
  (:require [clojure.core.match :refer [match]]
            [clojure.core.match.regex]
            [clojure.string :as str]))

(defn line->instruction
  "Parse the given line into an instruction"
  [line]
  (match [line]
    [#"noop"] [0 [:noop]]
    [#"addx (-?\d+)"] [1 [:addx (Integer/parseInt (first (re-find #"(-?\d+)" line)))]]))


(def crt-bounds
  "Static definition of the CRT bounds"
  {:x 40 :y 6})

(defn cycle->crt-coordinate
  "Convert a cycle number into a CRT Y X pair"
  [cycle]
  (let [x (rem (dec cycle) (:x crt-bounds))
        y (rem
            (quot (dec cycle) (:x crt-bounds))
            (:y crt-bounds))]
    [y x]))

(defn crt->str
  "Take the given CRT state and render it as a string"
  [{:keys [crt]}]
  (->> (for [y (range (:y crt-bounds))]
         (apply str (for [x (range (:x crt-bounds))]
                      (if (crt [y x])
                        "#" "."))))
       (str/join "\n")))

(defn run-crt-cycle
  [{:keys [cycle x] :as state}]
  (let [[_ x-pos :as coord] (cycle->crt-coordinate cycle)
        on?                 (<= (dec x-pos) x (inc x-pos))]
    (assoc-in state [:crt coord] on?)))

(defn run-cycle
  "Run the provided instruction against the given `state` and return the
  new state"
  [state]
  (let [instruction (first (:instructions state))
        state       (-> state
                        (update :cycle inc)
                        (update :instructions rest)
                        (run-crt-cycle))]
    (when instruction
      (match instruction
        [0 [:noop]]     state
        [0 [:addx num]] (update state :x + num)
        [x op]          (update state :instructions conj [(dec x) op])))))

(defn signal-strength
  "Return the signal strength of the provided `state` during the next cycle"
  [{:keys [cycle x]}]
  (* (inc cycle) x))

(defn init-state
  "Return a new computer state with the provided instructions"
  [instructions]
  {:x            1
   :cycle        0
   :crt          {}
   :instructions instructions})

(defn solve-part-one
  "Return the solution for part one"
  [input]
  (let [initial-state
        (init-state (map line->instruction (str/split-lines input)))
        states
        (iterate run-cycle initial-state)
        interesting-cylces [19 59 99 139 179 219]]
    (->> (for [cycle interesting-cylces]
           (nth states cycle))
         (map signal-strength)
         (apply +))))

(defn solve-part-two
  "Run the provided input and return the CRT output at termination"
  [input]
  (->> (str/split-lines input)
       (map line->instruction)
       (init-state)
       (iterate run-cycle)
       (take-while some?)
       last
       (crt->str)))

(comment
  (-> (slurp "resources/day_10.txt")
      (solve-part-one))

  (-> (slurp "resources/day_10.txt")
      (solve-part-two)
      (println))
  )
