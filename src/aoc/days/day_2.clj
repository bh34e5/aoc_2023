(ns aoc.days.day-2
  (:require
    [aoc.utils :as utils]
    [clojure.string :as s]))

(defn- read-as-lines []
  (utils/read-as-lines 2))

(defn- color-count->pair [color-count-str]
  (let [[count-str color] (s/split color-count-str #" ")
        count-long (parse-long count-str)
        color-kwd (keyword color)]
    [color-kwd count-long]))

(def ^:private base-color-count
  {:red 0
   :green 0
   :blue 0})

(defn- game-str->map [game-str]
  (let [color-counts (s/split game-str #", ")
        items (mapcat color-count->pair color-counts)
        new-map (apply hash-map items)]
    (into base-color-count new-map)))

(defn- parse-input [line]
  (let [[id-str items-str] (s/split line #": ")
        id (parse-long (last (s/split id-str #" ")))
        games (s/split items-str #"; ")]
    {:id id
     :games (map game-str->map games)}))

(defn- is-possible-game? [{:keys [red green blue]}]
  (and (<= red 12)
       (<= green 13)
       (<= blue 14)))

(defn part-1 []
  (let [lines (read-as-lines)
        games (map parse-input lines)
        valid-games (filter #(every? is-possible-game? (:games %1))
                            games)]
    (reduce + (map :id valid-games))))

(defn- min-set-from-game [game-map]
  (let [max-red (:red (apply max-key :red (:games game-map)))
        max-green (:green (apply max-key :green (:games game-map)))
        max-blue (:blue (apply max-key :blue (:games game-map)))]
    {:red max-red
     :green max-green
     :blue max-blue}))

(defn- set-power [{:keys [red green blue]}]
  (* red green blue))

(defn part-2 []
  (let [lines (read-as-lines)
        games (map parse-input lines)
        min-sets (map min-set-from-game games)]
    (reduce + (map set-power min-sets))))
