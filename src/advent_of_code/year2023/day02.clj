(ns advent-of-code.year2023.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-draw [line]
  (reduce (fn [[r g b] line]
            (let [[ns color] (string/split line #" ")
                  n (Long/parseLong ns)]
              (case color
                "red" [n g b]
                "green" [r n b]
                "blue" [r g n])))
    [0 0 0]
    line))

(defn parse-line [line]
  (let [[_ id rst] (re-find #"Game (\d+): (.+)" line)
        games (string/split rst #";")
        draws (map #(string/split (string/trim %) #"( ?), ") games)]
    [(Long/parseLong id)
     (map parse-draw draws)]))

(defn analyse-game [maxes [id draws :as game]]
  [id (every? true? (for [draw draws]
                      (every? true? (map <= draw maxes))))])

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (map parse-line)
      (doall))))

(defn part1 [maxes path]
  (reduce + (map first (filter second (map #(analyse-game maxes %) (parse path))))))

(defn part2-game [[id draws :as game]]
  (apply * (apply map max draws)))

(defn part2 [path]
  (->> (parse path)
    (map part2-game)
    (reduce +)))

(comment
  (part1 [12 13 14] "resources/2023/day02/demo.in")
  (part1 [12 13 14] "resources/2023/day02/problem.in")

  (part2 "resources/2023/day02/demo.in")
  (part2 "resources/2023/day02/problem.in"))