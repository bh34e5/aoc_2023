(ns aoc.days.day-8
  (:require
   [aoc.utils :as utils]
   [clojure.string :as s]))

(defn- read-as-lines []
  (utils/read-as-lines 8))

;;; example inputs
(comment
  (def t-in-1 "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)")

  (def t-in-2 "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

  (def t-in-3 (s/split-lines "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"))

  (parse-lines t-in-1)
  (parse-lines t-in-2)
  (def t1 (parse-lines t-in-1))
  (def t2 (parse-lines t-in-2))
  (def t3 (parse-lines t-in-3))

  (re-matches #"(\w+) = \((\w+), (\w+)\)" "AAA = (BBB, CCC)")
  (nth (re-matches #"(\w+) = \((\w+), (\w+)\)" "AAA = (BBB, CCC)") 1)

  (follow-map (:instructions t1) (:node-map t1))
  (follow-map (:instructions t2) (:node-map t2))
  (follow-map-grouped (:instructions t3)
                      (:node-map t3)
                      (initial-nodes (:node-map t3)))

  (every? ends-with-z? (vector :11Z :22Z))

  (let [cur-nodes
        ;(list :11Z :22Z)
        (initial-nodes (:node-map t3))
        instructions
        ;(list :right)
        (list :left :right)
        initial-insts instructions
        node-map (:node-map t3)
        steps 2]
    (cond
      (every? ends-with-z? cur-nodes) steps
      (empty? instructions) (list initial-insts
                                  node-map
                                  cur-nodes
                                  steps
                                  initial-insts)
      :else (list (rest instructions)
                  node-map
                  (mapv (partial follow-node
                                 node-map
                                 (first instructions))
                        cur-nodes)
                  (inc steps)
                  initial-insts)))

  nil)

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

;;; part 2

(defn- ends-with-a? [kwd]
  (= \A (last (name kwd))))

(defn- ends-with-z? [kwd]
  (= \Z (last (name kwd))))

(comment
  (defn- follow-map-grouped
    [instructions node-map cur-nodes]
    (loop [c-insts instructions
           cur-nodes cur-nodes
           steps 0]
      (cond
        (every? ends-with-z? cur-nodes) steps
        (empty? c-insts) (recur instructions
                                cur-nodes
                                steps)
        :else (recur (rest c-insts)
                     (mapv (partial follow-node
                                    node-map
                                    (first c-insts))
                           cur-nodes)
                     (inc steps))))))

(defn- follow-map-grouped
  ([instructions node-map cur-nodes]
   (follow-map-grouped instructions node-map cur-nodes 0 instructions))
  ([instructions node-map cur-nodes steps initial-insts]
   (cond
     (every? (memoize ends-with-z?) cur-nodes) steps
     (empty? instructions) (recur initial-insts
                                  node-map
                                  cur-nodes
                                  steps
                                  initial-insts)
     :else (recur (rest instructions)
                  node-map
                  (mapv (partial follow-node
                                 node-map
                                 (first instructions))
                        cur-nodes)
                  (inc steps)
                  initial-insts))))

(defn- initial-nodes [node-map]
  (filter ends-with-a? (keys node-map)))

(defn part-2 []
  (let [inp (load-input)
        {:keys [instructions node-map]} inp
        inits (initial-nodes node-map)]
    (follow-map-grouped instructions
                        node-map
                        inits)))

(comment
  (part-1)
  (part-2))
