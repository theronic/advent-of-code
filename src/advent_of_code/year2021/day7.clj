(ns advent-of-code.year2021.day7
  (:require [clojure.java.io :as io]
            [advent-of-code.year2021.utils :as u])
  (:import (java.io BufferedReader)))

(defn solve [cost-fn path]
  (with-open [rdr (io/reader path)]
    (let [xs     (->> (.readLine rdr) (re-seq #"(\d+),?") (map second) (map read-string))
          sorted (sort xs)
          cs     (for [x (range (first sorted) (inc (last sorted)))]
                   [(reduce + (map #(cost-fn % x) sorted)) x])
          sc     (sort-by first cs)]
      (take 10 sc))))

(defn part1 [path]
  (solve #(Math/abs (- %1 %2)) path))

(defn cost [a b]
  (reduce (fn [acc x] (+ acc x)) 0
    (range 1 (inc (Math/abs (- a b))))))

(defn part2 [path]
  (solve cost path))

(comment
  (part1 "resources/2021/day7/demo.in")
  (part1 "resources/2021/day7/problem.in")
  (part2 "resources/2021/day7/demo.in")
  (part2 "resources/2021/day7/problem.in"))
