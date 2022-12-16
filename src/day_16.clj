(ns day-16
  (:require [clojure.string :as str]))

(defn line->valve
  "Parse a line into a valve"
  [line]
  (let [valve     (second (re-find #"Valve (\w+)" line))
        flow-rate (-> (re-find #"flow rate=(\d+)" line)
                      (second)
                      (read-string))
        leads-to  (-> (re-find #"leads? to valves? (.*)" line)
                      (second)
                      (str/split #",")
                      (#(mapv str/trim %)))]
    {:valve valve :flow-rate flow-rate :leads-to leads-to :open false}))

(defn shortest-path
  "Finds the shortest path in `m` from `start` to `end`"
  [m start end]
  (loop [queue (into clojure.lang.PersistentQueue/EMPTY [start])
         state {start {:cost 0 :via nil}}]
    (let [node (peek queue)]
      (if (= node end)
        (->> node
             (iterate (comp :via state))
             (take-while some?)
             (reverse)
             (rest))
        (let [neighbors (->> (m node)
                             :leads-to
                             (remove (some-fn state (set queue))))]
          (recur
            (into (pop queue) neighbors)
            (reduce
              #(assoc %1 %2 {:cost (inc (:cost (state node))) :via node})
              state
              neighbors)))))))

(defn enrich-with-travel-times
  "Enrich each node in the valves map with a map of travel times to other valve"
  [m]
  (reduce
    (fn [acc [from to]]
      (assoc-in acc [from :travel-time to] (count (shortest-path m from to))))
    m
    (for [[k _]     m
          [other _] m
          :when     (not= k other)]
      [k other])))

(defn input->valves
  "Parse the provided `input` into a map of valves"
  [input]
  (-> (into {} (map (comp (juxt :valve identity) line->valve)) (str/split-lines input))
      (enrich-with-travel-times)))

(defn expected-values
  "Return the expected values of nodes in `m`, sorted descending"
  [m current-node-id mins-remaining]
  (->> (vals m)
       (remove (some-fn
                 :open
                 (comp zero? :flow-rate)))
       (keep
         (fn [{:keys [valve flow-rate]}]
           (let [travel-time    (get-in m [current-node-id :travel-time valve])
                 open-time      1
                 remaining-time (- mins-remaining travel-time open-time)]
             (when (pos? remaining-time)
               [valve (+ (* remaining-time flow-rate)
                         (-> (expected-values
                               (assoc-in m [valve :open] true)
                               valve
                               remaining-time)
                             first
                             second
                             (or 0)))]))))
       (sort-by second >)))

(comment
  (-> (slurp "resources/day_16.txt")
      (input->valves)
      (expected-values "AA" 30)
      first)
  )
