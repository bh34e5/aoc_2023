(ns aoc.days.day-4
  (:require
    [clojure.math :as math]
    [clojure.set :as set]
    [clojure.string :as str]
    [aoc.utils :as utils]))

(defn read-as-lines []
  (utils/read-as-lines 4))

;;; common

(defn- parse-line [line]
  (let [[card-str nums-str] (str/split line #":\s+")
        [wins mine] (str/split nums-str #"\s+\|\s+")
        card (parse-long (last (str/split card-str #"\s+")))
        win-nums (map parse-long (str/split wins #"\s+"))
        my-nums (map parse-long (str/split mine #"\s+"))]
    {:card card
     :wins (set win-nums)
     :mine (set my-nums)}))

(defn- parse-lines [lines]
  (map parse-line lines))

;;; part 1

(defn- card-score [{:keys [wins mine]}]
  (let [overlap (set/intersection wins mine)
        size (count overlap)]
    (if (= size 0)
      0
      (long (math/pow 2 (dec size))))))

(defn part-1 []
  (let [lines (read-as-lines)
        parsed (parse-lines lines)]
    (reduce + (map card-score parsed))))
