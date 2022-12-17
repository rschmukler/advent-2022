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

(defn expected-value
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
               (+ (* remaining-time flow-rate)
                  (-> (expected-value
                        (assoc-in m [valve :open] true)
                        valve
                        remaining-time)
                      (or 0)))))))
       (apply max 0)))

(def expected-values-part-2
  "Return the expected values of nodes in `m`, sorted descending"
  (memoize
    (fn [m [me el] [my-mins-remaining el-mins-remaining]]
      (let [valves-we-could-open-next (->> (vals m)
                                           (remove
                                             (some-fn
                                               :open
                                               (comp zero? :flow-rate))))]
        (apply
          max
          0
          (for [my-next valves-we-could-open-next
                el-next valves-we-could-open-next
                :when   (not= my-next el-next)
                :let    [my-travel-time (get-in m [me :travel-time my-next])
                         el-travel-time (get-in m [el :travel-time el-next])
                         my-rem-time    (dec (- my-mins-remaining my-travel-time))
                         el-rem-time    (dec (- el-mins-remaining el-travel-time))
                         new-state      (-> m
                                            (assoc-in [my-next :open] true)
                                            (assoc-in [el-next :open] true))
                         my-valve       (m my-next)
                         el-valve       (m el-next)]]
            (+ (if-not (pos? my-rem-time)
                 0
                 (* my-rem-time (:flow-rate my-valve)))
               (if-not (pos? el-rem-time)
                 0
                 (* el-rem-time (:flow-rate el-valve)))
               (expected-values-part-2 new-state [my-next el-next] [my-rem-time el-rem-time]))))))))

#_(def expected-value
    (memoize
      (fn [m current-node mins-remaining]
        (let [{:keys [leads-to flow-rate open]} (m current-node)]
          (cond
            (not (pos? mins-remaining)) 0
            (or open (zero? flow-rate))
            (->> leads-to
                 (map #(expected-value m % (dec mins-remaining)))
                 (apply max))
            :else
            (let [val-if-we-dont-open
                  (->>  leads-to
                        (map #(expected-value m % (dec mins-remaining)))
                        (apply max))
                  val-if-we-do-open
                  (->>  leads-to
                        (map #(expected-value (assoc-in m [current-node :open] true) % (- mins-remaining 2)))
                        (apply max)
                        (+ (* flow-rate (dec mins-remaining))))]
              (max val-if-we-do-open val-if-we-dont-open)))))))

#_(def expected-value-part-two
    (memoize
      (fn [m [me el] mins-remaining]
        (let [me-valve (m me)
              el-valve (m el)]
          (cond
            (not (pos? mins-remaining)) 0
            ()
            ))
        )))

(comment
  (time
    (-> (slurp "resources/day_16.txt")
        (input->valves)
        (expected-value "AA" 30)))

  (time
    (-> (slurp "resources/day_16.txt")
        (input->valves)
        (expected-value-part-2 ["AA" "AA"] [26 26])))

  ()
  )
