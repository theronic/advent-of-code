(ns advent-of-code.year2021.day15
  (:require [clojure.java.io :as io]
            [com.rpl.specter :as S]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (S/transform [S/ALL S/ALL] #(- (int %) 48) (mapv vec (line-seq rdr)))))

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

;(defn search-A*
;  ([graph state] (search-A* graph state 0 []))
;  ([graph state current-cost path]
;   (if (= goal state)
;     (let [path (conj path state)]
;       [path current-cost])
;     (if (not (empty? (get graph state)))
;       (let [costs (get-costs state current-cost)
;             next-state (get-next-state state costs)]
;         (recur next-state (get-real-cost current-cost state next-state) (conj path state)))))))

(defn pred->path
  "Determine the path to GOAL from the map of predecessors PRED."
  [pred goal]
  (loop [node goal, path ()]
    (if (contains? pred node)
      (recur (pred node) (cons node path))
      (cons node path))))

(defn expand
  [node h [open g pred] succ cost]
  (let [g-succ (+ (g node) cost)]
    (if (and (contains? open succ)
          (>= g-succ (g succ)))
      [open g pred]
      [(assoc open succ (+ g-succ (h succ)))
       (assoc g succ g-succ)
       (assoc pred succ node)])))

(defn a*
  "Determine the shortest path from START to GOAL in graph GRAPH
with heuristic cost function H. GRAPH format is {from {to cost}}."
  [start goal graph h]
  (loop [open   (priority-map start 0)
         closed ()
         g      {start 0}
         pred   {}]
    (if (empty? open)
      :no-path-found
      (let [node (key (peek open))]
        (if (= node goal)
          [:path-found (pred->path pred goal) (g goal)]
          (let [successors (apply dissoc (graph node) closed)
                [open* g* pred*] (reduce-kv (partial expand node h)
                                   [(pop open) g pred]
                                   successors)]
            (recur open* (conj closed node) g* pred*)))))))


(comment

  (a* :a :f
    {:a {:b 5 :c 6}
     :b {:c 7 :d 8 :e 9}
     :e {:f 10}}
    (fn [x] 10))

  ((juxt identity {:a 5}) :a)


  (part1 "resources/2021/day15/demo.in")
  (part1 "resources/2021/day15/problem.in")
  (part2 "resources/2021/day15/demo.in")
  (part2 "resources/2021/day15/problem.in"))