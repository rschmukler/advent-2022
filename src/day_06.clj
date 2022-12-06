(ns day-06)

(defn find-packet-start-ix
  "Find the packet start index for the given buffer size"
  [input buff-size]
  (-> (for [ix    (range buff-size (count input))
            :let  [slice (subs input (- ix buff-size) ix)]
            :when (= buff-size (count (set slice)))]
        ix)
      (first)))

(comment
  ;; Part One
  (-> (slurp "resources/day_06.txt")
      (find-packet-start-ix 4))

  ;; Part two
  (-> (slurp "resources/day_06.txt")
      (find-packet-start-ix 14)))
