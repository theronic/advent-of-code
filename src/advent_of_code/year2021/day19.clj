(ns advent-of-code.year2021.day19
  (:require [clojure.java.io :as io]
            [clojure.core.matrix :as mat]
            [com.rpl.specter :as S]
            [clojure.string :as string]))

;; Unsolved.

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (doall
      (into {}
        (for [[header & lines] (->> (line-seq rdr)
                                 (partition-by string/blank?)
                                 (remove #{'("")}))]
          [(Long/parseLong (first (rest (re-find #"--- scanner (\d+) ---" header))))
           (->> lines
             (mapv #(string/split % #","))
             (S/transform [S/ALL S/ALL] #(Long/parseLong %)))])))))
;(map #(Long/parseLong %)))])))))

(comment

  (Long/parseLong (first (rest (re-find #"--- scanner (\d+) ---" "--- scanner 4 ---"))))

  (Long/parseLong "-123")

  (re-find #"--- scanner (\d+) ---" "--- scanner 0 ---")

  (def (parse "resources/2021/day19/demo.in"))

  (mat/mul [[1 1] [2 2] [3 3]] [2 2])
  (mat/rotate [[1 1] [2 2] [3 3]] [1 0])

  (mat/mmul [[1 1] [2 2] [3 3]] [])

  (mat/rotate [[1 2 3 4 5 6]] 1 -1)


  (mat/mmul [[1 1 1]
             [2 2 2]
             [-3 -3 -3]]

    (mat/* (mat/identity-matrix 3) 3.0))

  (use 'clojure.core.matrix)
  (comment
    (+ [[1 2]
        [3 4]]
      (mat/mul (identity-matrix 2) 3.0))))

;;         | 1     0      0    |
;Rx(a) =   | 0  cos(a) -sin(a) |
;          | 0  sin(a)  cos(a) |

(defn rotation-matrix
  "Z forward?"
  [axis rads]
  (case axis
    x [[1 0 0]
       [0 (Math/cos rads) (- (Math/sin rads))]
       [0 (Math/sin rads) (Math/cos rads)]]
    y [[(Math/cos rads) 0 (Math/sin rads)]
       [0 1 0]
       [(- (Math/sin rads)) 0 (Math/cos rads)]]
    z [[(Math/cos rads) (- (Math/sin rads)) 0]
       [(Math/sin rads) (Math/cos rads) 0]
       [0 0 1]]))

(defn deg->rad [deg]
  (/ (* deg Math/PI) 180))

(comment
  (deg->rad 90)
  (S/transform [S/ALL S/ALL] inc [[1 2 3] [4 5 6]]))

;(Math/round -1.0)

(def rot-matrices (for [axis    '[x y z]
                        degrees [90 180 270]
                        :let [rads (deg->rad degrees)]]
                    [axis degrees (rotation-matrix axis rads)]))

(for [[axis degrees matrix] rot-matrices]
  (mat/mmul matrix [1 1 1]))

(defn rotate [matrix coll]
  (vec
    (for [v coll]
      (->> (mat/mmul v matrix)
        (S/transform [S/ALL] #(Math/round (double %)))))))

(defn rotations [coll]
  (let [matrices (for [axis    '[x y z]
                       degrees [90 180 270]
                       :let [rads (deg->rad degrees)]]
                   [axis degrees (rotation-matrix axis rads)])]
    (into {}
      (cons [['x 0] coll]                                   ;; identity. consider nil.
        (for [[axis degrees matrix] matrices]
          [[axis degrees]
           (rotate matrix coll)])))))

;(rotations [[1 1 1]])

(for [matrix (rotation-matrix 'x 0)]
  (mat/mmul matrix [1 1 1]))

;(do
;  (prn axis degrees mat)
;  [axis degrees
;   (for [v coll]
;     (->> (mat/mmul mat v)
;       (S/transform [S/ALL S/ALL] #(Math/round (double %)))))]))))

(take 1 (rotations [[1 2 3]]))

(let [vs       [[1 1 1] [2 2 2]]
      matrices (for [axis    '[x y z]
                     degrees [0 90 180 270]]
                 (rotation-matrix axis (deg->rad degrees)))]
  (for [mat matrices]
    (count
      (distinct
        (cons v
          (S/transform [S/ALL S/ALL] #(Math/round (double %))
            (mat/mmul matrices v)))))))

;(mat/mmul [1 1 1] [2 2 2] [1 1 1] [1 1 1])

(BigInteger. "111111111" 2)


(let [v        [2 2 2]
      matrices (for [axis    '[x y z]
                     degrees [0 90 180 270]]
                 (rotation-matrix axis (deg->rad degrees)))]
  (count
    (distinct
      (cons v
        (S/transform [S/ALL S/ALL] #(Math/round (double %))
          (mat/mmul matrices v))))))

(comment
  (mapv (fn [x] (mapv #(Math/round %) x)) (mat/mmul [[1 1 1]] (rotation-matrix 'x (deg->rad 90)))))

(defn align [s1 s2]
  ())

(defn with-offset [offset coll]
  (map #(mapv + offset %) coll))

(defn sqr [x]
  (* x x))

(defn rootsum [coll]
  (Math/sqrt (reduce + (map sqr coll))))

(defn distance [p1 p2]
  (Math/sqrt (reduce + (map sqr (mapv - p1 p2)))))

(defn distances [s1 s2]
  (for [p1 s1
        p2 s2]
    (distance p1 p2)))

(defn search [heuristic start visited])

(defn rotate [degrees coord]
  (case degrees
    90))

(comment

  ;-x => +y
  ;+y => x

  (mapv - [1 1 1] [2 2 2])

  (mapv - [404 -588 -901] [686 422 578])

  (filter (fn [[k v]]
            (> v 1))
          (frequencies [1 1 1 2 3]))

  (filter (comp #(>= % 2) second) (frequencies [1 1 2 3 4 5]))

  (get data 0)

  (loop [beacons (get data 0)
         colls (dissoc data 0)]
    (prn beacons)
    (if (seq colls)
      (let [matches (for [[sensor coll2] colls
                          [[axis degrees] rcoll] (rotations coll2)
                          :let [matches (->> (for [a beacons
                                                   b rcoll]
                                               (mapv - a b))
                                          (frequencies)
                                          (filter (comp #(>= % 12) second)))]
                          :when (seq matches)]
                      [matches
                       sensor
                       axis
                       degrees
                       rcoll])]
        (prn "got here" matches)
        (if (seq matches)
          (let [[[offset cnt]
                 sensor axis
                 degrees cnt points] (ffirst matches)
                pts' (for [p points] (map + p offset))
                beacons' (apply conj beacons pts')]
            (prn "match" offset "todo merge")
            (recur beacons' (rest colls)))
          (recur nil nil)))                                 ;(recur beacons (rest colls))))
      (do
        (prn "fail" beacons)
        ;; return failure mode?
        beacons)))
  ;[[sensor2 axis degrees]
  ; (->> (frequencies ds)
  ;   (filter (comp #(>= % 12) second)))]))

  (def *fs (for [[sensor1 coll1] data
                 [sensor2 coll2] (rest data)
                 :when (not= sensor1 sensor2)]
             (for [[[axis degrees] rcoll2] (rotations coll2)]
               (let [ds (for [a coll1
                              b rcoll2]
                          (mapv - a b))]
                 [[sensor1 sensor2 axis degrees]
                  (->> (filter (fn [[k v]]
                                 (>= v 12)) (frequencies ds)))]))))

  (first *fs)
  (second *fs)
             ;(filter
             ;  (fn [[k v]] (number? v))
             ;  fs)))
  ;(do ;(prn fs)
    ;    ;fs
    ;    (take 100 fs))); (map second fs)))) ;(filter (comp #(>= % 12) second) fs)))
  ;(filter (fn [[k v]]
    ;          (>= v 12)) fs))
  ;(filter (fn [[k v]] (>= v 12)) fs))
        ;(filter (fn [[k v] (>= v 12)]))])
    ;[[sensor1 sensor2] (map - coll1 coll2)])

  (sfirst data)
  (rotations (nfirst data))

  (second (first data))

  (rotations s1)

  (frequencies
    (for [[sensor1 coll1] data
          [sensor2 coll2] data]
      (for [[[axis degree] rcoll2] coll2]
        rcoll2)))
  ;(for [rotated ])
  ;    [[axis degrees] coll] (rotations (next (second data)) s2)
  ;    b coll]
  ;;rotation []]
  ;(mapv - a b)))
  (sort-by second (frequencies
                    (for [a s1
                          b s2
                          c (rotations b)]
                      ;rotation []]
                      (mapv - a b))))


  (sort-by second
    ;(filter (fn [[offset cnt]]))
    (frequencies
      (for [a s1
            [[axis degrees] coll] (rotations s2)
            b coll]
        (mapv - a b))))


  (mapv - [1 1] [4 3])

  (map #(mapv + [1 0] %) [[2 3] [4 5]])

  (def s1 [[0 2] [4 1] [3 3]])
  (def s2 [[-1 -1] [-5 0] [-2 1]])

  (distances s1 s2)

  (map #(reduce + %) '((1 2 3) (4 5 6)))

  (reverse (sort-by second (for [y (range -10 11)
                                 x (range -10 11)
                                 :let [s2' (with-offset [x y] s2)
                                       ds  (distances s1 s2')]]
                             [[x y] (reduce + ds)]))))
  ;(mapv #(reduce + %) (distances s1 s2'))))
