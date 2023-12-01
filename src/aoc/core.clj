(ns aoc.core
  (:gen-class))

(defn -main [day-num part-num]
  (let [package-str (str "aoc.days.day-" day-num)
        package (symbol package-str)]
    (require package :reload)
    (cond
      (= part-num 1) (eval (list (symbol package-str "part-1")))
      (= part-num 2) (eval (list (symbol package-str "part-2")))
      :else          (throw (ex-info "Invalid part number"
                                     {:part-num part-num})))))
