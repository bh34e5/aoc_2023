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

;;; part 2

(defn- additional-cards
  [grouped-cards vec-ct overlap-ct card]
  (let [copy-nums (map (partial + card)
                       (map inc (range overlap-ct)))
        card-arrs (map (partial get grouped-cards) copy-nums)
        dup-cards (map #(repeat vec-ct (first %1)) card-arrs)
        vectorized (mapcat #(vector (:card (first %1)) %1)
                           dup-cards)]
    vectorized))

(defn- process-copies [grouped-cards]
  (loop [grouped-cards grouped-cards
         card 1
         res {}]
    (let [card-vec (get grouped-cards card)]
      (if (<= card (count grouped-cards))
        (let [c (first card-vec)
              vec-ct (count card-vec)
              overlap (set/intersection (:wins c) (:mine c))
              overlap-ct (count overlap)
              copies (additional-cards grouped-cards
                                       vec-ct
                                       overlap-ct
                                       (:card c))
              copies-map (apply hash-map copies)
              merged (merge-with concat grouped-cards copies-map)]
          (recur merged
                 (inc card)
                 (into res (hash-map card card-vec))))
        res))))

(defn part-2 []
  (let [lines (read-as-lines)
        parsed (parse-lines lines)
        grouped (group-by :card parsed)
        processed (process-copies grouped)]
    (reduce + (map count (vals processed)))))
