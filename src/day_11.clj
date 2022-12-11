(ns day-11
  (:require [clojure.string :as str]
            [wing.core :as w]))

(defn line->ints
  "Parse out all ints in the line"
  [line]
  (->> (re-seq #"\d+" line)
       (map #(Integer/parseInt %))))

(defn lines->monkey
  "Parse a group of lines into a MONKEY"
  [lines]
  (let [[id-line
         items-line
         op-line
         test-line
         true-line
         false-line] lines]
    {:id        (first (line->ints id-line))
     :items     (vec (line->ints items-line))
     :inspected []
     :operation
     (into [(if (re-find #"\+" op-line)
              + *)]
           (concat
             (map keyword (re-seq #"old" op-line))
             (line->ints op-line)))
     :test
     [(first (line->ints test-line))
      (first (line->ints true-line))
      (first (line->ints false-line))]}))

(defn input->monkeys
  "Parse the provided input into monkeys indexed by ID"
  [input]
  (->> input
       (str/split-lines)
       (partition-by #{""})
       (remove #(= '("") %))
       (map lines->monkey)
       (w/index-by :id)))

(defn apply-operation
  "Apply the given operation to the worry level and return the new worry-level"
  [[op a b] worry-level]
  (op (if (= :old a) worry-level a)
      (if (= :old b) worry-level b)))

(defn divisible?
  "Return true if `num` is divisible by the divisor"
  [num divisor]
  (zero? (rem num divisor)))

(defn process-monkey-turn
  "Process the monkey with `id`'s turn and return the new monkeys."
  [worry-level-f monkeys id]
  (let [{:keys                             [operation items]
         [test-divisor true-tgt false-tgt] :test} (monkeys id)]
    (reduce
      (fn process-item [monkeys worry-level]
        (let [new-worry-level (-> (apply-operation operation worry-level)
                                  (worry-level-f))]
          (-> monkeys
              (update-in [id :inspected] conj worry-level)
              (update-in [(if (divisible? new-worry-level test-divisor)
                            true-tgt false-tgt) :items] conj new-worry-level))))
      (-> monkeys
          (assoc-in [id :inspected] [])
          (assoc-in [id :items] []))
      items)))

(defn process-round
  "Process a single round and return the new monkeys"
  [worry-level-f monkeys]
  (reduce
    (partial process-monkey-turn worry-level-f)
    monkeys
    (->> monkeys (keys) (sort))))

(defn inspection-counts
  "Return the number of inspection counts for the given monkeys after the provided
  number of rounds"
  [worry-level-f num-rounds monkeys]
  (->> (iterate (partial process-round worry-level-f) monkeys)
       (rest)
       (take num-rounds)
       (map #(update-vals % (comp count :inspected)))
       (apply merge-with +)))

(def part-one-worry-f
  "The worry modifier to solve part one"
  #(quot % 3))

(defn build-part-two-worry-f
  "Return the worry modifier to solve part two"
  [monkeys]
  (let [val (->> monkeys
                 (vals)
                 (map (comp first :test))
                 (apply *))]
    (fn [x]
      (mod x val))))

(defn solve-part-one
  [input]
  (->> input
       (input->monkeys)
       (inspection-counts part-one-worry-f 20)
       (vals)
       (sort >)
       (take 2)
       (apply *)))

(defn solve-part-two
  [input]
  (let [monkeys (input->monkeys input)]
    (->> monkeys
         (inspection-counts (build-part-two-worry-f monkeys) 10000)
         (vals)
         (sort >)
         (take 2)
         (apply *))))


(comment
  (solve-part-one (slurp "resources/day_11.txt")))
