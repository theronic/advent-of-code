(ns advent-of-code.year2021.day6
  (:require [clojure.java.io :as io]
            [com.rpl.specter :as S]))

(defn parse [lines]
  (let [nums (map #(Long/parseLong %)
               (clojure.string/split (first lines) #","))]
    (frequencies nums)))

(defn next-day [state]
  (let [spawn  (get state 0 0)
        state' (S/transform [S/MAP-KEYS] dec state)]
    (-> (dissoc state' -1)
      (assoc 6 (+ (get state' 6 0) spawn)
             8 (+ (get state' 8 0) spawn)))))

(defn solve [days path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (parse)
      (iterate next-day)
      (drop days)
      (first)
      (vals)
      (reduce +))))

(comment
  (solve 18 "resources/2021/day6/demo.in")
  (solve 80 "resources/2021/day6/demo.in")
  (solve 80 "resources/2021/day6/problem.in")
  (solve 256 "resources/2021/day6/problem.in"))