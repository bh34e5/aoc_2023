(ns aoc.days.day-6
  (:require
    [clojure.math :as math]
    [clojure.string :as str]
    [aoc.utils :as utils]))

(defn- read-as-lines []
  (utils/read-as-lines 6))

;;; common

(defn- get-times [lines]
  (let [time-line (first lines)
        times-str (second (str/split time-line #"Time:"))
        times (map parse-long (re-seq #"\d+" times-str))
        distance-line (second lines)
        distances-str (second (str/split distance-line #"Distance:"))
        distances (map parse-long (re-seq #"\d+" distances-str))]
    (interleave times distances)))

(defn- get-initial-guess [[target distance]]
  (let [dist-sqrt (long (math/sqrt distance))]
    (loop [a dist-sqrt
           b dist-sqrt]
      (if (= target (+ a b))
        [a b]
        (if (< distance (* a (inc b)))
          (recur (dec a)
                 (inc b))
          (recur a
                 (inc b)))))))
(defn- get-num-better [initial-guess]
  (let [[a b] initial-guess
        target (+ a b)
        half-change (- (long (/ target 2)) a)]
    (+ (* 2 half-change) (if (even? target) -1 0))))

;;; part 1

(defn part-1 []
  (let [lines (read-as-lines)
        times (get-times lines)]
    (->> times
         (partition 2)
         (map get-initial-guess)
         (map get-num-better)
         (reduce *))))

;;; part 2

(defn part-2 []
  (let [lines (read-as-lines)
        lines-clean (map #(str/replace %1 #"\s+" "") lines)
        times (get-times lines-clean)]
    (->> times
         (partition 2)
         (map get-initial-guess)
         (map get-num-better)
         (reduce *))))
