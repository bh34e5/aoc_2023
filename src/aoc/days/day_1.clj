(ns aoc.days.day-1)

(defn read-input-file []
  (slurp "inputs/day_1.txt"))

(defn read-as-lines []
  (let [contents (read-input-file)]
    (seq (.split contents "\n"))))

(defn get-calibration-from-line [line]
  (let [nums (filter #(Character/isDigit %1) line)
        calibration-str (str (first nums) (last nums))]
    (parse-long calibration-str)))

(defn part-1 []
  (let [lines (read-as-lines)]
    (reduce + (map get-calibration-from-line lines))))
