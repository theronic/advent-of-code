(ns advent-of-code.year2021.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :as S]))

(defn parse-point [line]
  (mapv #(Long/parseLong %) (rest (re-find #"(\d+),(\d+)" line))))

(defn parse-instruction [line]
  (let [[axis n] (rest (re-find #"fold along ([y|x])=(\d+)" line))]
    [(symbol axis) (Long/parseLong n)]))

(defn parse-input [lines]
  (let [[points instructions] (split-with (complement string/blank?) lines)]
    [(set (map parse-point points))
     (map parse-instruction (rest instructions))]))

(defn render-grid [point-set]
  (let [cols (inc (apply max (map first point-set)))
        rows (inc (apply max (map second point-set)))]
    (->> (for [y (range rows)
               x (range cols)
               :let [v (get point-set [x y])]]
           (if v \# \.))
      (partition cols)
      (map #(apply str %))
      (string/join \newline))))

(defn fold-up [n coll]
  (let [{top -1, bottom 1} (group-by (fn [[x y]] (.compareTo y n)) coll)]
    (clojure.set/union (set top)
      (set (map (fn [[x y]] [x (- n (- y n))]) bottom)))))

(defn fold-left [n coll]
  (let [{left -1, right 1} (group-by (fn [[x y]] (.compareTo x n)) coll)]
    (clojure.set/union (set left)
      (set (map (fn [[x y]] [(- n (- x n)) y]) right)))))

(defn folds [coll cmds]
  (reduce (fn [coll [axis n]]
            (case axis
              y (fold-up n coll)
              x (fold-left n coll)))
    coll cmds))

(defn part1 [path]
  (with-open [rdr (io/reader path)]
    (let [[coll cmds] (parse-input (line-seq rdr))]
      (count (folds coll (take 1 cmds))))))

(defn part2 [path]
  (with-open [rdr (io/reader path)]
    (->> (parse-input (line-seq rdr))
      (apply folds)
      (render-grid))))

(comment
  (part1 "resources/2021/day13/hard.in")
  (part1 "resources/2021/day13/demo.in")
  (part1 "resources/2021/day13/problem.in")
  (print (part2 "resources/2021/day13/demo.in"))
  (print (part2 "resources/2021/day13/hard.in"))
  (print (part2 "resources/2021/day13/problem.in")))
