(ns advent-of-code.year2020.day1
  (:require [clojure.string :as string]))

(defn parse [in]
  (map #(Long/parseLong %) (string/split-lines in)))

(defn part1 [input]
  (for [x input
        y input
        :when (= 2020 (+ x y))]
    [(* x y) x y]))

(defn part2 [input]
  (for [x input
        y input
        z input
        :when (= 2020 (+ x y z))]
    [(* x y z) x y z]))

(comment
  (part1 (parse (slurp "resources/2020/day1/demo.in")))
  (part1 (parse (slurp "resources/2020/day1/problem.in")))
  (part2 (parse (slurp "resources/2020/day1/demo.in")))
  (part2 (parse (slurp "resources/2020/day1/problem.in"))))
