(ns advent-of-code.year2021.day5
  (:require [advent-of-code.year2021.utils :as u]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (->> (re-find #"(\d+),(\d+) -> (\d+),(\d+)" line)
    (rest)
    (map #(Long/parseLong %))
    (partition 2)
    (map vec)
    (vec)))

(defn helper [a b]
  (cond (= a b) b
        (< a b) (inc a)
        (> a b) (dec a)))

(defn fill-line
  "Returns a lazy seq of coordinates from [a b] until [c d] is reached."
  [[[x1 y1] [x2 y2]]]
  (cons [x1 y1]
    (if (not= [x1 y1] [x2 y2])
      (lazy-seq (fill-line [[(helper x1 x2) (helper y1 y2)] [x2 y2]])))))

(defn solve [filter-fn path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (map parse-line)
      (filter filter-fn)
      (map fill-line)
      (apply concat)
      (frequencies)
      (filter (fn [[_ v]] (> v 1)))
      (count))))

(defn part1 [path]
  (solve (fn [[[x1 y1] [x2 y2]]]
           (or (= x1 x2) (= y1 y2)))
    path))

(defn part2 [path]
  (solve identity path))

(comment
  (part1 "resources/2021/day5/demo.in")
  (part1 "resources/2021/day5/problem.in")
  (part2 "resources/2021/day5/demo.in")
  (part2 "resources/2021/day5/problem.in"))
