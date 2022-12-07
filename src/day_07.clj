(ns day-07
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))


(defn line->value
  "Parse a single line of input into something a bit more friendly to match on"
  [line]
  (let [cd                             (second (re-find #"\$ cd (.*)" line))
        ls?                            (re-find #"\$ ls$" line)
        dir                            (second (re-find #"^dir (.*)" line))
        [file-size file-name :as file] (rest (re-find #"(\d+) (.*)" line))]
    (cond
      (some? cd)   [:cd cd]
      (some? ls?)  [:ls]
      (some? dir)  [:dir dir]
      (some? file) [:file file-name (Integer/parseInt file-size)])))

(defn ->path
  "Utility function to build paths for `get-in` and `assoc-in`"
  [& items]
  (flatten items))

(defn input->dirmap
  "Return the directory map for the given input"
  [input-text]
  (-> (reduce
        (fn [{:keys [pwd] :as acc} line]
          (match (line->value line)
            [:cd ".."] (update acc :pwd pop)
            [:cd path] (update acc :pwd conj path)
            [:ls]      acc
            [:dir dir] (assoc-in acc (->path :result pwd dir) {})
            [:file file size] (assoc-in acc (->path :result pwd file) size)))
        {:result {} :pwd []}
        (str/split-lines input-text))
      (get-in [:result "/"])))

(defn dir?
  "Return whether the provided `item` is a directory (ie. not a file)"
  [item]
  (map? item))

(defn file?
  "Return whether the provided `item` is a file (ie. not a directory)"
  [item]
  (number? item))

(defn ->size
  "Return the size of the given file, or all files in the directory"
  [dir-or-file]
  (if (file? dir-or-file)
    dir-or-file
    (reduce + 0 (map ->size (vals dir-or-file)))))

(defn solve-part-one
  [text-input]
  (->> (input->dirmap text-input)
       (tree-seq dir? vals)
       (filter dir?)
       (map ->size)
       (filter #(<= % 100000))
       (reduce + 0)))

(defn solve-part-two
  [text-input]
  (let [dirmap         (input->dirmap text-input)
        min-size       30000000
        total-size     70000000
        used-size      (->size dirmap)
        available-size (- total-size used-size)]
    (->> (tree-seq dir? vals dirmap)
         (filter dir?)
         (filter #(<= min-size (+ available-size (->size %))))
         (map ->size)
         (sort)
         first)))
(comment
  (solve-part-one (slurp "resources/day_07.txt"))
  (solve-part-two (slurp "resources/day_07.txt"))
  )
