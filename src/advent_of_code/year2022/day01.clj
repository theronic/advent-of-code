(ns advent-of-code.year2022.day01
  (:require [clojure.java.io :as io]
            [com.rpl.specter :as S]
            [clojure.string :as string]
            [advent-of-code.year2022.utils :as u]))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (partition-by string/blank?)
      (remove #(= '("") %))
      (S/transform [S/ALL S/ALL] u/parse-int)
      (S/transform [S/ALL] u/sum))))

(defn part1 [path]
  (->> (parse path) (apply max)))

(defn part2 [path]
  (->> (parse path)
    (sort)
    (reverse)
    (take 3)
    (u/sum)))

(comment
  (part1 "resources/2022/day01/demo.in")
  (part2 "resources/2022/day01/demo.in")

  (part1 "resources/2022/day01/problem.in")
  (part2 "resources/2022/day01/problem.in"))
