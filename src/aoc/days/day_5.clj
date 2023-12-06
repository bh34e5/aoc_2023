(ns aoc.days.day-5
  (:require
    [clojure.string :as str]
    [aoc.utils :as utils]))

(defn- read-as-lines []
  (utils/read-as-lines 5))

;;; common

(defn- parse-map-name [line]
  (let [mapping (first (str/split line #" "))
        [from to] (str/split mapping #"-to-")]
    {:name mapping
     :from (keyword from)
     :to (keyword to)}))

(defn- parse-map [lines]
  (let [map-name (parse-map-name (first lines))]
    (loop [entries []
           lines (rest lines)]
      (if (and (seq lines) (seq (first lines)))
        (let [[dest source rng] (re-seq #"\d+" (first lines))]
          (recur (conj entries
                       {:dest (parse-long dest)
                        :source (parse-long source)
                        :range (parse-long rng)})
                 (rest lines)))
        [(into map-name {:entries entries}) (rest lines)]))))

(defn- parse-input [lines]
  (let [seeds-line (first lines)
        map-lines (rest (rest lines))
        seeds (map parse-long
                   (re-seq #"\d+" (last (str/split seeds-line #": "))))]
    (loop [maps []
           map-lines map-lines]
      (if (seq map-lines)
        (let [[this-map next-lines] (parse-map map-lines)]
          (recur (conj maps this-map) next-lines))
        {:maps maps
         :seed seeds}))))

(defn- map-number [entries number]
  (loop [entries entries]
    (if (seq entries)
      (let [etr (first entries)
            dif (- number (:source etr))]
        (if (<= 0 dif (dec (:range etr)))
          (+ dif (:dest etr))
          (recur (rest entries))))
      number)))

(defn- follow-map
  [keyed-maps current-key input-nums]
  (if-let [maps (get keyed-maps current-key)]
    (let [cur-map (first maps)
          res (map (partial map-number (:entries cur-map)) input-nums)]
      {:vals res
       :next (:to cur-map)})
    {:vals input-nums
     :next nil}))

(defn- follow-maps
  [keyed-maps start input-nums]
  (loop [cur-key start
         inps input-nums]
    (let [{res :vals next-key :next} (follow-map keyed-maps cur-key inps)]
      (if next-key
        (recur next-key
               res)
        res))))

;;; part 1

(defn part-1 []
  (let [lines (read-as-lines)
        inp (parse-input lines)
        keyed-maps (group-by :from (:maps inp))
        res (follow-maps keyed-maps :seed (:seed inp))]
    (apply min res)))
