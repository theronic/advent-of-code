(ns advent-of-code.year2022.day05
  (:require [clojure.string :as string]))

(defn parse-line [line]
  (map second (re-seq #"\[(.)\] |    " line)))

(defn parse-move [line]
  (if-some [[_ n from to] (re-find #"move (\d+) from (.) to (.)" line)]
    [(Integer/parseInt n) from to]))

(defn part1-move [stack head] (into stack head))
(defn part2-move [stack head] (concat head stack))

(defn move [move-fn stacks n from to]
  (let [[head tail] (split-at n (stacks from))]
    (-> stacks
      (assoc from tail)
      (update to move-fn head))))

(defn pad-space [s] (str s " "))

(defn pad-coll [coll n el]
  (concat coll (repeat (- n (count coll)) el)))

(defn solve [move-fn path]
  (let [[stacks _ moves] (partition-by string/blank?
                           (string/split-lines (slurp path)))
        labels  (map string/trim (string/split (last stacks) #"   "))
        stacks  (butlast stacks)
        parsed  (map (comp parse-line pad-space) stacks)
        longest (apply max (map count parsed))
        padded  (map #(pad-coll % longest nil) parsed)
        cols    (map #(drop-while nil? %) (apply map list padded))
        moves   (map parse-move moves)
        grid    (zipmap labels cols)
        solved  (reduce (fn [acc [n from to]]
                          (move move-fn acc n from to)) grid moves)]
    (->> (map solved #(solved %) labels)
      (map first)
      (string/join))))

(comment
  (solve part1-move "resources/2022/day05/demo.in")
  (solve part1-move "resources/2022/day05/problem.in")

  (solve part2-move "resources/2022/day05/demo.in")
  (solve part2-move "resources/2022/day05/problem.in"))