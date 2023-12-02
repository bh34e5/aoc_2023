(ns aoc.days.day-1
  (:require [aoc.utils :as utils]))

(defn- read-as-lines []
  (utils/read-as-lines 1))

;;; common

(def ^:private strs-to-vals
  {"one"   \1
   "two"   \2
   "three" \3
   "four"  \4
   "five"  \5
   "six"   \6
   "seven" \7
   "eight" \8
   "nine"  \9})

(defn- get-digit-from-str [line direction]
  (let [check-fn  (if (= direction ::fwd)
                    clojure.string/starts-with?
                    clojure.string/ends-with?)
        default-fn (if (= direction ::fwd)
                     first
                     last)]
    (loop [ks (keys strs-to-vals)]
      (let [check-key (first ks)]
        (if (seq ks)
          (if (check-fn line check-key)
            (get strs-to-vals check-key)
            (recur (rest ks)))
          (default-fn line))))))

(defn- get-substrings [line direction]
  (letfn [(get-bounds [ind] (case direction
                              ::fwd [ind (count line)]
                              ::rev [0 (- (count line) ind)]))]
    (for [ind (range (count line))]
      (let [[start stop] (get-bounds ind)]
        (.substring line start stop)))))

(defn- replace-num-strs [line direction]
  (let [substrs (get-substrings line direction)
        combiner (fn [cur add-char]
                   (case direction
                     ::fwd (str cur add-char)
                     ::rev (str add-char cur)))]
    (loop [substrs substrs
          rebuilt-str ""]
      (if (seq substrs)
        (recur (rest substrs)
               (combiner rebuilt-str
                         (get-digit-from-str (first substrs) direction)))
        rebuilt-str))))

(defn- get-replaced-strs [line]
  (let [f (replace-num-strs line ::fwd)
        r (replace-num-strs line ::rev)]
    [f r]))

;;; part 1

(defn- get-calibration-from-line-1 [line]
  (let [nums (filter #(Character/isDigit %1) line)
        calibration-str (str (first nums) (last nums))]
    (parse-long calibration-str)))

(defn part-1 []
  (let [lines (read-as-lines)]
    (reduce + (map get-calibration-from-line-1 lines))))

;;; part 2

(defn- get-calibration-from-line-2 [line]
  (let [[f r] (get-replaced-strs line)
        char1 (first (filter #(Character/isDigit %1) f))
        char2 (last  (filter #(Character/isDigit %1) r))]
    (parse-long (str char1 char2))))

(defn part-2 []
  (let [lines (read-as-lines)]
    (reduce + (map get-calibration-from-line-2 lines))))
