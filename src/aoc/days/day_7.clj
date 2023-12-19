(ns aoc.days.day-7
  (:require
   [clojure.string :as str]
   [aoc.utils :as utils]))

(defn- read-as-lines []
  (utils/read-as-lines 7))

;;; common

(defn- parse-lines [lines]
  (for [line lines]
    (let [[cards score-str] (str/split line #" ")
          score (parse-long score-str)]
      {:cards (seq cards)
       :cards-str cards
       :score score})))

(defn- load-input []
  (let [lines (read-as-lines)]
    (parse-lines lines)))

(def ^:private cards-1
  [\A \K \Q \J \T \9 \8 \7 \6 \5 \4 \3 \2])

(def ^:private card-to-rank-1
  (apply hash-map
         (apply concat
                (map-indexed (fn [ind card] [card ind])
                             cards-1))))

(defn- cards-comparator* [c-1 c-2]
  (- (get card-to-rank-1 c-1) (get card-to-rank-1 c-2)))

(defn- cards-list-comparator* [l-1 l-2]
  (loop [l-1 l-1
         l-2 l-2]
    (if (and (seq l-1) (seq l-2))
      (let [c (cards-comparator* (first l-1) (first l-2))]
        (if (= 0 c)
          (recur (rest l-1) (rest l-2))
          c))
      0)))

(def ^:private ordered-hands-freqs
  '[(5) (1 4) (2 3) (1 1 3) (1 2 2) (1 1 1 2) (1 1 1 1 1)])

(def ^:private hands-to-rank
  (apply hash-map
         (apply concat
                (map-indexed (fn [ind freqs] [freqs ind])
                             ordered-hands-freqs))))

(defn- frequencies-comparator* [f-1 f-2]
  (let [n-1 (vals f-1)
        n-2 (vals f-2)
        s-1 (sort n-1)
        s-2 (sort n-2)]
    (- (get hands-to-rank s-1) (get hands-to-rank s-2))))

(defn- hand-comparator* [card-1 card-2]
  (let [c-1 (:cards card-1)
        c-2 (:cards card-2)
        f-1 (frequencies c-1)
        f-2 (frequencies c-2)
        f-comp (frequencies-comparator* f-1 f-2)]
    (cond
      (not (= 0 f-comp)) f-comp
      :else              (cards-list-comparator* c-1 c-2))))

;;; part 1

(defn- get-cards-and-ranks []
  (let [inp (load-input)
        sorted (sort hand-comparator* inp)
        ct (count sorted)]
    (loop [sorted sorted
           i ct
           res []]
      (if (seq sorted)
        (recur (rest sorted)
               (dec i)
               (conj res (into (first sorted) {:rank i})))
        res))))

(defn part-1 []
  (let [ranked (get-cards-and-ranks)]
    (->> ranked
         (map #(* (:rank %1) (:score %1)))
         (reduce +))))

;;; part 2
;;;
;;; starting with a re-write of a bit of stuff...

(def ^:private cards-2
  [\A \K \Q \T \9 \8 \7 \6 \5 \4 \3 \2 \J])

(def ^:private card-to-rank-2
  (apply hash-map
         (apply concat
                (map-indexed (fn [ind card] [card ind])
                             cards-2))))

(defn- freqs->str [freqs]
  (apply str (mapcat #(repeat (get freqs %1) %1)
                     (keys freqs))))

(defn- replace-first [cstr]
  (for [card (remove #{\J} cards-2)]
    (str/replace-first cstr \J card)))

(defn- permute-freqs [freqs]
  (let [cstr (freqs->str freqs)]
    (loop [i (get freqs \J)
           others (list cstr)]
      (if (zero? i)
        others
        (recur (dec i)
               (mapcat replace-first others))))))

(defn- max-with [f & colls]
  (:orig (first
          (sort-by :val
                   (apply map
                          (fn [& args]
                            {:val (apply f args)
                             :orig args})
                          colls)))))

(defn- hand-type [hand-str]
  (let [fs (frequencies hand-str)
        vs (vals fs)
        sorted-vs (sort vs)]
    (get hands-to-rank sorted-vs)))

(defn- card-types [hand-str]
  (map #(get card-to-rank-2 %1) (seq hand-str)))

(declare maximize-frequencies)

(defn- hand-power [hand]
  (let [{:keys [cards-str]} hand
        max-hand (maximize-frequencies (frequencies cards-str))
        max-str (freqs->str max-hand)
        h-type (hand-type max-str)
        fc-types (card-types cards-str)]
    (reduce (fn [acc cur]
              (+ (* (count cards-2) acc)
                 cur))
            (concat (list h-type) fc-types))))

(defn- replace-and-get-max [freqs]
  (frequencies (first (max-with hand-type (permute-freqs freqs)))))

(defn- maximize-frequencies [freqs]
  (let [no-jacks (dissoc freqs \J)]
    (cond
      (= no-jacks freqs) freqs ;; frequencies didn't contain jacks, do nothing
      (= {} no-jacks)    freqs ;; there were only jacks, 5 of a kind is max
      :else              (replace-and-get-max freqs))))

(defn part-2 []
  (let [inp (load-input)
        sorted (sort-by (memoize hand-power) inp)
        revd (reverse sorted)
        mapped (map-indexed (fn [idx v] (* (inc idx) (:score v))) revd)]
    (reduce + mapped)))
