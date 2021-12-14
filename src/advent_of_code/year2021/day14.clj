(ns advent-of-code.year2021.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-rule [line]
  (let [[_ k v] (re-find #"(.+) -> (.+)" line)]
    [(vec k) (first v)]))

(defn next-step [rules [letters fs]]
  (reduce (fn [[letters acc :as prev] [pair cnt]]
            (if (pos? cnt)
              (if-let [v (get rules pair)]
                [(update letters v (fnil (partial + cnt) 0))
                 (let [[a b] pair]
                   (-> acc
                     (update pair (fnil - 0) cnt)
                     (update [a v] (fnil + 0) cnt)
                     (update [v b] (fnil + 0) cnt)))]
                prev)
              prev))
    [letters fs] fs))

(defn solve [n path]
  (with-open [rdr (io/reader path)]
    (let [[template _ & more] (doall (line-seq rdr))
          rules   (into {} (map parse-rule more))
          letters (frequencies template)
          pairs   (frequencies (map vec (partition 2 1 template)))
          [output _] (nth (iterate (partial next-step rules) [letters pairs]) n)
          vs      (vals output)]
      (- (apply max vs) (apply min vs)))))

(defn part1 [path] (solve 10 path))

(defn part2 [path] (solve 40 path))

(comment
  (part1 "resources/2021/day14/demo.in")
  (part1 "resources/2021/day14/problem.in")

  (part2 "resources/2021/day14/demo.in")
  (part2 "resources/2021/day14/problem.in"))
