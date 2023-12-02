(ns aoc.utils)

(defn- read-input-file [day-num]
  (slurp (str "inputs/day_" day-num ".txt")))

(defn read-as-lines [day-num]
  (let [contents (read-input-file day-num)]
    (clojure.string/split-lines contents)))
