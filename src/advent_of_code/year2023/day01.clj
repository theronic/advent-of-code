(ns advent-of-code.year2023.day01
  (:require [clojure.java.io :as io]))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (into [] (line-seq rdr))))

(def digit? #{\1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn solve [line]
  (Long/parseLong (apply str ((juxt first last) (filter digit? line)))))

(defn part1 [path]
  (->> (parse path)
    (map solve)
    (reduce +)))

(defn ->int [term]
  (or ({"one"   1
        "two"   2
        "three" 3
        "four"  4
        "five"  5
        "six"   6
        "seven" 7
        "eight" 8
        "nine"  9} term)
    (- (int (first term)) 48)))

(defn part2-line [line]
  (Long/parseLong
    (->> line
      (re-seq #"(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))")
      (map second)
      ((juxt first last))
      (map ->int)
      (apply str))))

(defn part2 [path]
  (->> (parse path)
    (map part2-line)
    (reduce +)))

(comment
  (part1 "resources/2023/day01/demo.in")
  (part2 "resources/2023/day01/demo2.in")

  (part1 "resources/2023/day01/problem.in")
  (part2 "resources/2023/day01/problem2.in"))