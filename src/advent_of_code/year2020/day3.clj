(ns advent-of-code.year2020
  (:require [advent-of-code.year2021.utils :as u]
            [clojure.string :as string]))

(defn solve [path [dx dy]]
  (u/with-reader [rdr path]
    (let [lines  (into [] (line-seq rdr))
          width  (count (first lines))]
      (->> (iterate #(map + % [dy dx]) [dy dx])
        (map (fn [[y x]] (get-in lines [y (mod x width)])))
        (take-while identity)
        (filter #{\#})
        (count)))))

(defn part1 [path] (solve path [3 1]))

(defn part2 [path]
  (reduce * (map #(solve path %) [[1 1] [3 1] [5 1] [7 1] [1 2]])))

(comment
  (part1 "resources/2020/day3/demo.in")
  (part1 "resources/2020/day3/problem.in")
  (part2 "resources/2020/day3/demo.in")
  (part2 "resources/2020/day3/problem.in"))
