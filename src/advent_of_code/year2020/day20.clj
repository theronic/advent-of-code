(ns advent-of-code.year2020.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [com.rpl.specter :as S]))

; Solved.

;; for every tile, enumerate other tiles which have matching edges.

;; corners should have two edges with no matches and two edges with exactly one match.
;; i.e. 2x1, 2x2.

;(take 5 (iterate (fn [tile] (map reverse (apply map vector tile)))
;          (partition 3 (range 9))))
;
;((0 1 2)
; (3 4 5)
; (6 7 8))
;
;((6 3 0)
; (7 4 1)
; (8 5 2))
;
;
;(map reverse (apply map vector (partition 3 (range 9))))
;
;(defn rotate-tile [tile]
;  ["#.#.#####."
;   ".#..######"
;   "..#......."
;   "######...."
;   "####.#..#."
;   ".#...#.##."
;   "#.#####.##"
;   "..#.###..."
;   "..#......."
;   "..#.###..."])

;(defn parse [])
;(with-open [rdr (io/reader "resources/2020/day20/demo.in")]
;  (let [lines      (line-seq rdr)
;        tile-lines (remove #(= % '("")) (partition-by string/blank? lines))
;        tiles      (->> tile-lines
;                     (map (fn [[head & more]]
;                            (let [id    (Long/parseLong (second (re-find #"Tile (\d+):" head)))
;                                  edges [(first more)
;                                         (apply str (map last more))
;                                         (last more)
;                                         (apply str (map first more))]
;                                  redges (map string/reverse edges)]
;                              {:id    id
;                               :tile  (vec more)
;                               :edges (concat edges redges)}))))
;        m          (into {} (map (juxt :id identity) tiles))
;        grp        (->> tiles
;                     (mapcat (fn [tile]
;                               (for [edge (:edges tile)]
;                                 [edge (:id tile)])))
;                     (group-by first))]
;    (prn "cnt:" (count tiles))
;    (let [cnt (S/transform [S/MAP-VALS] count grp)
;          t1 (for [tile tiles]
;               [(:id tile)
;                (for [edge (:edges tile)]
;                  (get cnt edge 0))])]
;      {:tiles   tiles
;       :cnt     cnt
;       :t1      t1
;       :t2      (sort-by second (map (fn [[k v]] [k (reduce + v)]) t1))
;       :grp     grp
;       :corners (filter (fn [[k vs]]
;                          (= 2 (count vs))) grp)
;       :sides   (filter (fn [[k vs]]
;                          (= 3 (count vs))) grp)
;       ;:f1 (filter (fn [[k vs]]
;       ;              (= 1 (count vs))) grp)
;       :f       (map second (map second (map second (filter (fn [[k vs]]
;                                                              (>= (count vs) 2)) grp))))})))
;(split-with)
;
;(* 1327 1087 2753 1009)

(let [ms       (cons '(9 0) (partition 2 (range 10)))
      shuffled (shuffle ms)
      vars     (repeatedly 5 logic/lvar)]
  (logic/run 10 [q]
    (== q vars)
    (logic/everyg #(logic/membero % ms) vars)
    ;(logic/everyg #(fd/in % (apply fd/domain ms)) vars)
    (fd/distinct vars)))

(cons '(9 0) (partition 2 (range 10)))

(defn parse [[head & more]]
  (let [id     (Long/parseLong (second (re-find #"Tile (\d+):" head)))
        edges  [(first more)
                (apply str (map last more))
                (last more)
                (apply str (map first more))]
        redges (map string/reverse edges)]
    {:id    id
     :tile  (vec more)
     :edges (concat edges redges)}))

(with-open [rdr (io/reader "resources/2020/day20/problem.in")]
  (let [lines      (line-seq rdr)
        tile-lines (remove #(= % '("")) (partition-by string/blank? lines))]
    (mapv parse tile-lines)))

(parse)

(defn testo [l]
  [(if (= 1 1) succeed fail)])
  ;(conde
  ;  [(if (even? l) fail)]
  ;  [(if (odd? l) succeed)]))

(loop [path []
       frontier])


(run 10 [q]
  (fresh [a]
    (fd/in a (fd/domain 1 2 3))
    ;(conde)
    (== a q)))
  ;(testo q))
  ;(testo q))

(defn find-corners [tiles]
  (let [m   (into {} (map (juxt :id identity) tiles))
        grp (->> tiles
              (mapcat (fn [tile]
                        (for [edge (:edges tile)]
                          [edge (:id tile)])))
              (group-by first))]
    (let [cnt (S/transform [S/MAP-VALS] count grp)
          t1  (for [tile tiles]
                [(:id tile)
                 (for [edge (:edges tile)]
                   (get cnt edge 0))])
          orted (sort-by second (map (fn [[k v]] [k (reduce + v)]) t1))]
      {:corners (take 4 (sort-by second (map (fn [[k v]] [k (reduce + v)]) t1)))})))

;(defn nonconseco [l]
;  (conde
;    ;; empty list is non-consecutive
;    [(== l ())]
;    ;; singleton list is non-consecutive
;    [(fresh [x] (== l (list x)))]
;    ;; the real work
;    [(fresh [lhead lsecond ltail]
;       (conso lhead ltail l)
;       (secondo l lsecond)
;       (!= lhead lsecond)
;       (nonconseco ltail))]))

(use 'clojure.core.logic)

(defn secondo [l s]
  (fresh [x]
    (resto l x)
    (firsto x s)))

(defn neighbouro [l]
  (conde
    [(== l ())]
    [(fresh [x] (== l (list x)))]
    [(fresh [lhead lsecond ltail]
       (conso lhead ltail l)
       (secondo l lsecond)
       (fd/<= lhead lsecond)
       (neighbouro ltail))]))

;(fd/<)

(run* [q]
  (fresh [a]
    (conde
      [(== 1 a)]
      [(== 2 a)]
      [(== 3 a)])
    (== q a)))

(run* [q]
  (permuteo [3 1 2 2 5 1 2 3 4 6 7] q)
  (pred q #(every? (partial apply <=) (partition 2 1 %))))
  ;(neighbouro q))

(with-open [rdr (io/reader "resources/2020/day20/demo.in")]
  (let [lines      (line-seq rdr)
        tile-lines (remove #(= % '("")) (partition-by string/blank? lines))
        tiles      (map parse tile-lines)
        corners    (find-corners tiles)


        m          (into {} (map (juxt :id identity) tiles))
        grp        (->> tiles
                     (mapcat (fn [tile]
                               (for [edge (:edges tile)]
                                 [edge (:id tile)])))
                     (group-by first))]
    (let [cnt (S/transform [S/MAP-VALS] count grp)
          t1  (for [tile tiles]
                [(:id tile)
                 (for [edge (:edges tile)]
                   (get cnt edge 0))])]
      (reduce * (map first (take 4 (sort-by second (map (fn [[k v]] [k (reduce + v)]) t1))))))))

(with-open [rdr (io/reader "resources/2020/day20/problem.in")]
  (let [lines      (line-seq rdr)
        tile-lines (remove #(= % '("")) (partition-by string/blank? lines))
        tiles      (map parse tile-lines)
        corners    (find-corners tiles)
        m          (into {} (map (juxt :id identity) tiles))
        grp        (->> tiles
                     (mapcat (fn [tile]
                               (for [edge (:edges tile)]
                                 [edge (:id tile)])))
                     (group-by first))]
    (let [cnt (S/transform [S/MAP-VALS] count grp)
          t1  (for [tile tiles]
                [(:id tile)
                 (for [edge (:edges tile)]
                   (get cnt edge 0))])]
      (reduce * (map first (take 4 (sort-by second (map (fn [[k v]] [k (reduce + v)]) t1))))))))
       ;:grp     grp
       ;:corners (filter (fn [[k vs]]
       ;                   (= 2 (count vs))) grp)
       ;:sides   (filter (fn [[k vs]]
       ;                   (= 3 (count vs))) grp)
       ;:f1 (filter (fn [[k vs]]
       ;              (= 1 (count vs))) grp)
       ;:f       (map second (map second (map second (filter (fn [[k vs]]
       ;                                                       (>= (count vs) 2)) grp))))})))

(count (count (distinct (map second (map first (map second (count (:f1 *t))))))) (:tiles *t))
(doall (remove #(= % '("")) (partition-by string/blank? lines)))

(re-find #"Tile (\d+):" "Tile 2729:")


(logic/run 10 [q]
  (logic/fresh [a b]
    (logic/== q [a b])
    (fd/in a (fd/domain 1 2 3 4))
    (logic/project [a]
      (logic/membero a [2 3])
      (logic/== a b))))

(run 10 [q]
  (fresh [t1 t2 t3
          t4 t5 t6
          t7 t8 t9])
  (fd/in t1 t2 t3 t4 t5 t6 t7 t8 t9 (fd/domain 1951 1427 1489 2473 1951 2473 1427 1427 2971))
  (logic/== q [t1 t2 t3
               t4 t5 t6
               t7 t8 t9]))

(logic/run 10 [q]
  (logic/fresh [t1 t2 t3
                t4 t5 t6
                t7 t8 t9]
    (fd/in t1 t2 t3 t4 t5 t6 t7 t8 t9 (fd/interval 1 9))
    (logic/== t1)
    ;(logic/project [t1]
    ;  (logic/== t1 (inc t2)))
    ;(fd/in t1 t2 t3 t4 t5 t6 t7 t8 t9 (fd/domain 1951 1427 1489 2473 1951 2473 1427 1427 2971))
    (logic/== q [t1 t2 t3
                 t4 t5 t6
                 t7 t8 t9])))

;; n-queens


(ns m-clj.c7.nqueens
  (:require [clojure.core.logic :as l]))

;;; Example 7.2

(l/defne safeo [q qs]
  ([_ ()])
  ([[x1 y1] [[x2 y2] . t]]
   (l/!= x1 x2)
   (l/!= y1 y2)
   (l/project [x1 x2 y1 y2]
     (l/!= (- x2 x1) (- y2 y1))
     (l/!= (- x1 x2) (- y2 y1)))
   (safeo [x1 y1] t)))

(l/defne nqueenso [n qs]
  ([_ ()])
  ([n [[x y] . t]]
   (nqueenso n t)
   (l/membero x (range n))
   (safeo [x y] t)))

(defn solve-nqueens [n]
  (l/run* [qs]
    (l/== qs (map vector (repeatedly l/lvar) (range n)))
    (nqueenso n qs)))

(map vector (repeatedly l/lvar) (range 4))

(solve-nqueens 4)
(solve-nqueens 8)

(map vector (repeatedly lvar) (range 8))

(solve-nqueens 4)