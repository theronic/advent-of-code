(ns advent-of-code.year2022.day04
  (:require [com.rpl.specter :as S]
            [clojure.string :as string]))

(defn parse-line [line]
  (->> (string/split line #",")
    (map #(string/split % #"-"))
    (S/transform [S/ALL S/ALL] #(Integer/parseInt %))
    (S/transform [S/ALL S/LAST] inc) ;; inclusive range end.
    (map #(apply range %))
    (map set)))

(defn solve [f path]
  (->> (slurp path)
    (string/split-lines)
    (map parse-line)
    (filter f)
    (count)))

(defn subsumes? [[s1 s2]]
  (or (clojure.set/subset? s1 s2)
    (clojure.set/subset? s2 s1)))

(defn part1 [path]
  (solve subsumes? path))

(defn overlaps? [[s1 s2]]
  (seq (clojure.set/intersection s1 s2)))

(defn part2 [path]
  (solve overlaps? path))

(comment
  (part1 "resources/2022/day04/demo.in")
  (part1 "resources/2022/day04/problem.in")

  (part2 "resources/2022/day04/demo.in")
  (part2 "resources/2022/day04/problem.in"))
