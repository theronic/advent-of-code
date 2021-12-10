(ns advent-of-code.year2021.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse
  "Returns first invalid char or stack if line is incomplete."
  [tokens]
  (reduce (fn [stack token]
            (if-let [expected ({\( \), \[ \], \{ \}, \< \>} token)]
              (conj stack expected)
              (if (= token (peek stack))
                (pop stack)
                (reduced token))))
    '() tokens))

(defn part1 [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (map parse)
      (filter char?)
      (map {\) 3, \] 57, \} 1197, \> 25137})
      (reduce +))))

(defn part2 [path]
  (with-open [rdr (io/reader path)]
    (let [scores (->> (line-seq rdr)
                   (map parse)
                   (filter list?)
                   (map #(reduce (fn [acc token]
                                   (+ (* 5 acc) ({\) 1, \] 2, \} 3, \> 4} token)))
                           0 %))
                   (sort))]
      (nth scores (int (/ (count scores) 2))))))

(comment
  (part1 "resources/2021/day10/demo.in")
  (part2 "resources/2021/day10/demo.in")

  (part1 "resources/2021/day10/problem.in")
  (part2 "resources/2021/day10/problem.in"))