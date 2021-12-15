(ns advent-of-code.year2021.day15
  (:require [clojure.java.io :as io]
            [com.rpl.specter :as S]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (S/transform [S/ALL S/ALL] #(- (int %) 48) (vec (map vec (line-seq rdr))))))

(defn adjacent [[y x]]
  (map #(mapv + [y x] %) [[-1 0] [0 -1] [0 1] [1 0]]))

(defn lookup
  [grid [y x]]
  (let [numrows (count grid)
        numcols (count (first grid))
        qy      (quot y numrows)
        qx      (quot x numcols)]
    (if (and (< qy 5) (< qx 5))
      (if-let [v (get-in grid
                   [(rem y numrows)
                    (rem x numcols)])]
        (-> (dec v) (+ qy qx) (mod 9) (inc))))))

(defn dijkstra
  [start f]
  (loop [q (priority-map start 0)
         r {}]
    (if-let [[v d] (peek q)]
      (let [dist (->> (f v)
                   (S/setval [S/MAP-KEYS r] S/NONE)
                   (S/transform [S/MAP-VALS] (partial + d)))]
        (recur (merge-with min (pop q) dist) (assoc r v d)))
      r)))

(defn solve [path lookup-fn]
  (let [grid (parse path)
        f    (fn [[y x]]
               (->> (adjacent [y x])
                 (filter #(lookup-fn grid %))
                 (map (fn [loc] [loc (lookup-fn grid loc)]))
                 (into {})))]
    (-> (sort-by first (dijkstra [0 0] f))
      (last)                                                ;; right-bottom most cell happens to be last.
      (second))))

(defn part1 [path] (solve path get-in))

(defn part2 [path] (solve path lookup))

(comment
  (part1 "resources/2021/day15/demo.in")
  (part1 "resources/2021/day15/problem.in")
  (part2 "resources/2021/day15/demo.in")
  (part2 "resources/2021/day15/problem.in"))