(ns advent-of-code.year2021.day9
  (:require [advent-of-code.year2021.utils :as u]
            [com.rpl.specter :as S]))

(defn parse-grid [path]
  (u/with-reader [rdr path]
    (->> (line-seq rdr)
      (map vec)
      (S/transform [S/ALL S/ALL] #(- (int %) 48))
      (vec))))

(defn adjacent [[y x]]
  [[y (inc x)]
   [y (dec x)]
   [(inc y) x]
   [(dec y) x]])

(defn lowest? [grid [y x]]
  (let [v (get-in grid [y x])]
    (->> (adjacent [y x])
      (map #(get-in grid %))
      (remove nil?)
      (every? #(< v %)))))

(defn part1 [grid]
  (let [[height width] [(count grid) (count (first grid))]]
    (->> (for [y (range height)
               x (range width)]
           [y x])
      (filter #(lowest? grid %)))))

(defn find-basin [grid start]
  (loop [frontier [start]
         visited  #{start}]
    (if-let [loc (peek frontier)]
      (let [successors (->> (adjacent loc)
                         (remove #(contains? #{9 nil} (get-in grid %))))]
        (recur
          (apply conj (pop frontier) (remove visited successors))
          (apply conj visited successors)))
      visited)))

(defn part2 [path]
  (let [grid   (parse-grid path)
        lowest (part1 grid)]
    (->> lowest
      (map #(find-basin grid %))
      (map count)
      (sort)
      (take-last 3)
      (reduce *))))

(comment
  (part1 (parse-grid "resources/2021/day9/demo.in"))
  (part1 (parse-grid "resources/2021/day9/problem.in"))
  (part2 "resources/2021/day9/demo.in")
  (part2 "resources/2021/day9/problem.in"))
