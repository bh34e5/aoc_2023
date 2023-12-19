(ns aoc.days.day-8
  (:require [aoc.utils :as utils]))

(defn- read-as-lines []
  (utils/read-as-lines 8))

;;; common

(defn- parse-node [node-str]
  (let [matches (re-matches #"(\w+) = \((\w+), (\w+)\)"  node-str)]
    {:node-name (keyword (nth matches 1))
     :left      (keyword (nth matches 2))
     :right     (keyword (nth matches 3))}))

(defn- parse-nodes [nodes]
  (map parse-node nodes))

(defn- parse-lines [lines]
  (let [instructions (first lines)
        nodes-pre (rest (rest lines))
        nodes (parse-nodes nodes-pre)]
    {:instructions (map #(get {\R :right \L :left} %1) instructions)
     :nodes nodes
     :node-map (apply hash-map
                      (mapcat #(vector (:node-name %1) %1)
                              nodes))}))

(defn- load-input []
  (let [lines (read-as-lines)]
    (parse-lines lines)))

;;; part 1

(defn- follow-node [node-map cur-inst cur-node]
  (let [cur-node-complete (get node-map cur-node)]
    (get cur-node-complete cur-inst)))

(defn- follow-map
  ([instructions node-map]
   (follow-map instructions node-map :AAA 0 instructions))
  ([instructions node-map cur-node steps initial-insts]
   (cond
     (= :ZZZ cur-node) steps
     (empty? instructions) (recur initial-insts
                                  node-map
                                  cur-node
                                  steps
                                  initial-insts)
     :else (recur (rest instructions)
                  node-map
                  (follow-node node-map (first instructions) cur-node)
                  (inc steps)
                  initial-insts))))

(defn part-1 []
  (let [inp (load-input)
        {:keys [instructions node-map]} inp]
    (follow-map instructions node-map)))
