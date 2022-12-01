(ns advent-of-code.year2021.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :as S]))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (let [[algo _ & more] (into [] (line-seq rdr))]
      [algo (vec more)])))

(defn pos->bin [algo idx]
  ({\# 1 \. 0} (get algo idx)))

(def offsets [[-1 -1] [-1 0] [-1 1] [0 -1] [0 0] [0 1] [1 -1] [1 0] [1 1]])

(defn adjacent [[y x]]
  (map #(mapv + [y x] %) offsets))

(defn kernel [grid x])

;(pos->bin (first (parse)) 34)

(def *algo (first (parse "resources/2021/day20/problem.in")))
(def *grid (second (parse "resources/2021/day20/problem.in")))

(get-in *grid [2 1])

(BigInteger. (apply str (map (comp {\# \1 \. \0} #(get-in *grid %)) (adjacent [2 2]))) 2)

(let [grid *grid]
  ;(map {\# 1 \. 0})                                         ;(partial pos->bin *algo)
  (for [y [2]                                               ;(range (count grid))
        x [2] ;(range (count (first grid)))
        :let [cells (adjacent [y x])
              vs (map (comp {\# 1 \. 0} #(get-in grid %)) cells)]]
    (pos->bin *algo (BigInteger. (apply str (map (comp {\# \1 \. \0} #(get-in *grid %)) (adjacent [y x]))) 2))))

(defn grow-grid
  "pads the thing"
  [n char grid]
  (let [row-count (count grid)
        col-count (count (first grid))]
    (vec
      (concat
        (repeat n
          (vec (repeat (+ col-count (* 2 n)) char)))
        (vec (for [row grid]
               (vec (concat (repeat n char) row (repeat n char)))))
        (repeat n
          (vec (repeat (+ col-count (* 2 n)) char)))))))

(defn shrink-grid [n grid]
  (if (zero? n)
    grid
    (->> grid
      (mapv rest)
      (mapv butlast)
      (rest)
      (vec)
      (butlast)
      (vec)
      (map vec)
      (vec)
      (shrink-grid (dec n)))))

(comment
  (->> [[\# \. \#]
        [\. \# \.]
        [\# \. \#]]
    (grow-grid 2 \.))
    ;(shrink-grid 2)
    ;(pprint/pprint))
    ;(shrink-grid)
    ;(shrink-grid)
    ;(pprint/pprint))

  (->> [[\# \. \#]
        [\. \# \.]
        [\# \. \#]]
    (grow-grid)
    (shrink-grid)
    (shrink-grid)
    (pprint/pprint))

  (->> [[\# \# \#]
        [\# \. \#]
        [\# \# \#]]
    (grow-grid 10 \.)
    (do-thing *algo)
    (do-thing *algo)
    ;(shrink-grid)
    ;(shrink-grid)
    ;(do-thing *algo)
    ;(shrink-grid)
    ;(shrink-grid)
    (pprint/pprint)))

(defn do-thing [algo empty-char grid]
  (let [grid grid] ;(grow-grid \. grid)]
    (vec
      (for [y (range (count grid))]
        (vec
          (for [x (range (count (first grid)))
                :let [cells (adjacent [y x])
                      bits  (map (comp {\# \1 \. \0} #(get-in grid % empty-char)) cells)]]
            (nth algo (BigInteger. (apply str bits) 2))))))))

(comment
  (get *algo 511)

  (BigInteger. "111111111" 2)
  (get-in *grid [-1 -1] \0)

  (require '[clojure.pprint :as pprint])

  (pprint/pprint)

  ;(pprint/pprint)
  (def *grid2 (let [grid (grow-grid *grid)]
                (vec
                  (for [y (range (count grid))]
                    (vec
                      (for [x (range (count (first grid)))
                            :let [cells (adjacent [y x])
                                  bits  (map (comp {\# \1 \. \0} #(get-in grid % \.)) cells)]]
                        (nth algo (BigInteger. (apply str bits) 2))))))))

  (pprint/pprint *grid3)

  (let [[algo grid] (parse "resources/2021/day20/problem.in")]
    ;(pprint/pprint (grow-grid grid))
    ;(pprint/pprint (do-thing algo (do-thing algo grid)))) ;(do-thing algo grid))))
  ;[algo grid]
  ;(do-thing algo)
  ;(S/transform [S/ALL S/ALL] #{})
    (->> (do-thing algo grid)
      ;(do-thing algo)
      (pprint/pprint)))
      ;(S/select [S/ALL S/ALL])
      ;(filter #{\#})
      ;(count)))

  ;; 5913

  (let [[algo grid] (parse "resources/2021/day20/problem.in")]
    (get algo (BigInteger. "111111111" 2)))

  ;(grow-grid 4 \. (transient (second (parse "resources/2021/day20/demo.in"))))

  ;; 77793 is too high.

  ;; solution. grows continual. can probably be stabilised.
  (let [[algo grid] (parse "resources/2021/day20/problem.in")]
    ;[algo grid]
    ;(do-thing algo)
    ;(S/transform [S/ALL S/ALL] #{})
    (->> [grid \.]
      (iterate (fn [[grid char]]
                 (let [grid' (->> grid
                               (grow-grid 2 char)
                               (do-thing algo char))]
                               ;(shrink-grid 2))]
                   [grid' ({\. \#, \# \.} char)])))
      (drop 50)
      (ffirst) ;; extra first to drop char.
      ;(first)
      ;(map println)
      (S/select [S/ALL S/ALL])
      (filter #{\#})
      (count)))
    ;  (do-thing algo)))

  (let [[algo grid] (parse "resources/2021/day20/problem.in")]
    (->> (do-thing algo grid)
      (shrink-grid)
      (do-thing algo)
      (S/select [S/ALL S/ALL])
      (filter #{\#})
      (count))))

;(parse)