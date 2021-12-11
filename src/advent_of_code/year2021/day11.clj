(ns advent-of-code.year2021.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :as S]))

(defn parse-grid [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (mapv vec)
      (S/transform [S/ALL S/ALL] #(- (int %) 48)))))

(def offsets [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn adjacent [[y x]]
  (map #(mapv + [y x] %) offsets))

(defn find-locs [pred grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :let [v (get-in grid [y x])]
        :when (pred v)]
    [y x]))

(defn update-in-grid [grid f locs]
  (reduce (fn [grid loc] (update-in grid loc f)) grid locs))

(defn next-flash [flashed grid]
  (let [over10 (find-locs #(>= % 10) grid)
        grid'  (S/transform [S/ALL S/ALL] #(mod % 10) grid)]
    (if (seq over10)
      (let [neighbours (mapcat (fn [loc]
                                 (->> (adjacent loc)
                                   (filter #(get-in grid %))))
                         over10)]
        (next-flash
          (apply conj flashed over10)
          (update-in-grid grid' inc neighbours)))
      [flashed grid'])))

(defn step-seq [grid]
  (let [grid'  (S/transform [S/ALL S/ALL] inc grid)
        [flashed grid] (next-flash #{} grid')
        grid'' (update-in-grid grid (fn [_] 0) flashed)]
    (cons grid'' (lazy-seq (step-seq grid'')))))

(defn part1 [path]
  (let [grid (parse-grid path)]
    (->> (take 100 (step-seq grid))
      (reduce (fn [acc grid]
                (let [cnt (count (S/select [S/ALL S/ALL zero?] grid))]
                  (+ acc cnt)))
        0))))

(defn zero-grid? [grid] (every? zero? (S/select [S/ALL S/ALL] grid)))

(defn part2 [path]
  (let [grid (parse-grid path)]
    (->> (take-while (complement zero-grid?) (step-seq grid))
      (count)
      (inc))))

(comment
  (part1 "resources/2021/day11/demo.in")
  (part1 "resources/2021/day11/problem.in")
  (part2 "resources/2021/day11/demo.in")
  (part2 "resources/2021/day11/problem.in"))