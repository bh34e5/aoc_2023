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

(defn- range-end [rng]
  (+ (:source rng) (:range rng)))

(defn- ranges-distinct? [r-one r-two]
  (or (<= (range-end r-two) (:source r-one))
      (<= (range-end r-one) (:source r-two))))

(defn- ranges-intersects? [r-one r-two]
  (not (ranges-distinct? r-one r-two)))

(defn- range-contained? [outer inner]
  (<= (:source outer)
      (:source inner)
      (dec (range-end inner))
      (dec (range-end outer))))

(defn- split-range-on-intersection [entry rng]
  (cond
    ;; range contained in entry; no need to split
    (range-contained? entry rng)
    [rng]
    ;; entry contained in range; need to split range in three
    (range-contained? rng entry)
    [{:source (:source rng)
      :range (- (:source entry) (:source rng))}
     {:source (:source entry)
      :range (:range entry)}
     {:source (range-end entry)
      :range (- (range-end rng)
                (range-end entry))}]
    ;; entry runs into range; need to split range in two
    (<= (:source entry) (:source rng))
    [{:source (:source rng)
      :range (- (range-end entry)
                (:source rng))}
     {:source (range-end entry)
      :range (- (range-end rng)
                (range-end entry))}]
    ;; range runs into entry; need to split range in two the other way
    :else
    [{:source (:source rng)
      :range (- (:source entry)
                (:source rng))}
     {:source (:source entry)
      :range (- (range-end rng)
                (:source entry))}]))

(defn- split-ranges-on-intersection [ranges entry]
  (loop [ranges ranges
         res []]
    (if (seq ranges)
      (if (ranges-distinct? entry (first ranges))
        (recur (rest ranges)
               (conj res (first ranges)))
        (recur (rest ranges)
               (into []
                     (concat res
                             (split-range-on-intersection entry
                                                          (first ranges))))))
      res)))

(defn- map-number [entries number]
  (loop [entries entries]
    (if (seq entries)
      (let [etr (first entries)
            dif (- number (:source etr))]
        (if (<= 0 dif (dec (:range etr)))
          (+ dif (:dest etr))
          (recur (rest entries))))
      number)))

(defn- map-range [entries rng]
  (let [split-ranges (reduce split-ranges-on-intersection [rng] entries)]
    (loop [unmapped split-ranges
           next-entries entries
           results []]
      (if (seq unmapped)
        (if (seq next-entries)
          (if (range-contained? (first next-entries) (first unmapped))
            (recur (rest unmapped)
                   entries
                   (conj results {:source (+ (:dest (first next-entries))
                                             (- (:source (first unmapped))
                                                (:source (first next-entries))))
                                  :range (:range (first unmapped))}))
            (recur unmapped
                   (rest next-entries)
                   results))
          (recur (rest unmapped)
                 entries
                 (conj results (first unmapped))))
        results))))

(defn- map-number-or-range [entries number-or-range]
  (if (map? number-or-range)
    (map-range entries number-or-range)
    (map-number entries number-or-range)))

(defn- follow-map
  [keyed-maps current-key input-nums ranges?]
  (if-let [maps (get keyed-maps current-key)]
    (let [cur-map (first maps)
          mapped (map (partial map-number-or-range (:entries cur-map))
                      input-nums)
          res (if ranges? (apply concat mapped) mapped)]
      {:vals res
       :next (:to cur-map)})
    {:vals input-nums
     :next nil}))

(defn- follow-maps
  [keyed-maps start input-nums & {:keys [ranges?] :or {ranges? false}}]
  (loop [cur-key start
         inps input-nums]
    (let [{res :vals next-key :next} (follow-map keyed-maps
                                                 cur-key
                                                 inps
                                                 ranges?)]
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

;;; part 2

(defn- replace-seeds [parsed]
  (let [par (partition 2 (:seed parsed))
        all (map (fn [[source len]] {:source source :range len}) par)
        upd (into parsed {:seed all})]
    upd))

(defn part-2 []
  (let [lines (read-as-lines)
        inp-pre (parse-input lines)
        inp (replace-seeds inp-pre)
        keyed-maps (group-by :from (:maps inp))
        res (follow-maps keyed-maps :seed (:seed inp) :ranges? true)]
    (apply min (map :source res))))
