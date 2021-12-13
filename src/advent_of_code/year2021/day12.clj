(ns advent-of-code.year2021.day12
  (:require [clojure.java.io :as io]
            [com.rpl.specter :as S]
            [clojure.string :as string]))

(defn parse-line [line]
  (->> line (re-find #"(.+)-(.+)") (rest) (vec)))

(defn parse-graph [lines]
  (reduce (fn [graph [k v]]
            (-> graph
              (update k (fnil conj #{}) v)
              (update v (fnil conj #{}) k)))
    {} (map parse-line lines)))

(defn small-cave? [node] (every? #(Character/isLowerCase %) node))

(defn make-dfs [graph goal prune-neighbours]
  (fn search [path visited]
    (if (= goal (peek path))
      [path]
      (->> (peek path)
        (get graph)
        (remove #{"start"})
        (prune-neighbours visited)
        (mapcat #(search (conj path %)
                   (cond-> visited
                     (small-cave? %) (update % (fnil inc 0)))))))))

(defn find-paths [graph goal prune-neighbours]
  ((make-dfs graph goal prune-neighbours)
   ["start"] {}))

(defn solve [path prune-neighbours]
  (with-open [rdr (io/reader path)]
    (let [graph (parse-graph (line-seq rdr))]
      (find-paths graph "end" prune-neighbours))))

(defn part1 [path]
  (count (solve path (fn [visited neighbours]
                       (remove #(contains? visited %) neighbours)))))

(defn part2 [path]
  (count (solve path (fn [visited neighbours]
                       (if (some #(> % 1) (vals visited))
                         (remove #(contains? visited %) neighbours)
                         neighbours)))))

(comment
  (part1 "resources/2021/day12/demo.in")
  (part1 "resources/2021/day12/demo2.in")
  (part1 "resources/2021/day12/demo3.in")
  (part1 "resources/2021/day12/problem.in")

  (part2 "resources/2021/day12/demo.in")
  (part2 "resources/2021/day12/demo2.in")
  (part2 "resources/2021/day12/demo3.in")
  (part2 "resources/2021/day12/problem.in"))
