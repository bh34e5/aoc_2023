(ns aoc.days.day-3
  (:require [aoc.utils :as utils]))

(defn- read-as-lines []
  (utils/read-as-lines 3))

;;; common

(defn- parse-line [row line]
  (loop [line line
         symbols []
         part-nums []
         reading false
         cur-num 0
         start-ind 0
         col 0]
    (if (seq line)
      (if (Character/isDigit (first line))
        (recur (rest line)
               symbols
               part-nums
               true
               (+ (* 10 cur-num) (Character/digit (first line) 10))
               (if reading start-ind col)
               (inc col))
        (let [is-period? (= \. (first line))
              upd-symbs (if-not is-period?
                          (conj symbols {:row row
                                         :col col
                                         :sym (first line)})
                          symbols)]
          (if reading
            (recur (rest line)
                   upd-symbs
                   (conj part-nums {:start start-ind
                                    :end col
                                    :row row
                                    :num cur-num})
                   false
                   0
                   0
                   (inc col))
            (recur (rest line)
                   upd-symbs
                   part-nums
                   false
                   cur-num
                   start-ind
                   (inc col)))))
      (let [fin-parts (if reading
                        (conj part-nums {:start start-ind
                                         :end col
                                         :row row
                                         :num cur-num})
                        part-nums)]
        {:parts fin-parts
         :symbols symbols}))))

(defn- parse-input [lines]
  (let [mapped (map-indexed parse-line lines)]
    (apply merge-with concat mapped)))

(defn- group-symbols [symbols]
  (loop [symbols symbols
         res {}]
    (if (seq symbols)
      (let [cur (first symbols)]
        (recur (rest symbols)
               (assoc-in res [(:row cur) (:col cur)] cur)))
      res)))

(defn- neighbors [row col-start col-end]
  (for [r-plus (range 3)
        col-plus (range (+ 2 (- col-end col-start)))
        :when (not (and (= r-plus 1)
                        (< 0 col-plus (+ 1 (- col-end col-start)))))]
    {:row (+ row (dec r-plus))
     :col (+ col-start (dec col-plus))}))

(defn- part-overlaps?
  [{prow :row pstart :start pend :end}
   {crow :row ccol :col}]
  (and (= prow crow)
       (<= pstart ccol (dec pend))))

(defn- get-surrounding-parts [parts sym]
  (let [scol (:col sym)
        nbors (neighbors (:row sym) scol (inc scol))]
    (filter (fn [part]
              (some #(part-overlaps? part %1) nbors))
            parts)))

;;; part 1

(defn part-1 []
  (let [lines (read-as-lines)
        parsed (parse-input lines)
        grouped (group-symbols (:symbols parsed))
        neighbored (filter (fn [{:keys [start end row]}]
                             (some (fn [nbor]
                                     (get-in grouped [(:row nbor) (:col nbor)]))
                                   (neighbors row start end)))
                           (:parts parsed))]
    (reduce + (map :num neighbored))))

;;; part 2

(defn- is-gear? [parts sym]
  (and (= (:sym sym) \*)
       (= 2 (count (get-surrounding-parts parts sym)))))

(defn- get-gear-ratio [[g1 g2]]
  (* (:num g1) (:num g2)))

(defn part-2 []
  (let [lines (read-as-lines)
        parsed (parse-input lines)
        parts (:parts parsed)
        gears (filter #(is-gear? parts %1)
                      (:symbols parsed))
        all-surroundings (map #(get-surrounding-parts parts %1)
                              gears)
        gear-ratios (map get-gear-ratio
                         all-surroundings)]
    (reduce + gear-ratios)))
