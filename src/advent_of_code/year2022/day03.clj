(ns advent-of-code.year2022.day03
  (:require [clojure.java.io :as io]
            [advent-of-code.year2022.utils :as u :refer (sum)]
            [hyperfiddle.rcf :refer (tests tap %)]))

(defn ->priority [ch]
  (let [i (int ch)]
    (cond
      (<= (int \a) i (int \z)) (- i 96)
      (<= (int \A) i (int \Z)) (- i 38))))

(defn part1-line [line]
  (let [[left right] (split-at (int (/ (count line) 2)) line)
        both (clojure.set/intersection (set left) (set right))]
    (sum (map ->priority both))))

(defn part1 [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (map part1-line)
      (sum))))

(defn part2 [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (map #(map ->priority %))
      (partition 3)
      (map #(apply clojure.set/intersection (map set %)))
      (map first)
      (sum))))

(comment
  (part1 "resources/2022/day03/demo.in")
  (part2 "resources/2022/day03/demo.in")

  (part1 "resources/2022/day03/problem.in")
  (part2 "resources/2022/day03/problem.in"))