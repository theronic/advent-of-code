(ns advent-of-code.year2021.day1
  (:require [clojure.java.io :as io]
            [advent-of-code.year2021.utils :as u]))

(defn part1 [xs]
  (->> xs
    (partition 2 1)
    (filter (fn [[a b]] (> b a)))
    (count)))

(defn part2 [xs]
  (->> xs
    (partition 3 1)
    (map #(reduce + %))
    (part1)))

(comment
  (part1 (u/int-seq (u/scanner (io/reader "resources/2021/day1/demo.in"))))
  (part1 (u/int-seq (u/scanner (io/reader "resources/2021/day1/problem.in"))))
  (part2 (u/int-seq (u/scanner (io/reader "resources/2021/day1/problem.in")))))
