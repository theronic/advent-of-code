(ns advent-of-code.year2023.day03
  (:require [clojure.java.io :as io]))

(defn parse-1 [path]
  (with-open [rdr (io/reader path)]
    (into [] (line-seq rdr))))

(def digit? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn map-symbol? [ch]
  (not ((conj digit? \.) ch)))

(defn find-symbols [[line1 :as grid]]
  (let [cols (count line1)
        rows (count grid)]
    (set
      (for [y (range rows)
            x (range cols)
            :let [cell (get-in grid [y x])]
            :when (map-symbol? cell)]
        [y x]))))

(defn find-gears [[line1 :as grid]]                         ;; same as above. diff prd
  (let [cols (count line1)
        rows (count grid)]
    (set
      (for [y (range rows)
            x (range cols)
            :let [cell (get-in grid [y x])]
            :when (#{\*} cell)]
        [y x]))))

(def offsets
  (for [y [-1 0 1]
        x [-1 0 1]
        :when (not= x y 0)]
    [y x]))

(defn find-digits-around [grid coord]
  (let [coords (for [o offsets] (mapv + o coord))]
    (filter #(digit? (get-in grid %)) coords)))

(defn find-adjacent-numbers [[line1 :as grid] coords]
  (map #(find-digits-around grid %) coords))

(defn re-offsets
  "Returns [start end] offsets in line"
  [row re s]
  (let [m (re-matcher re s)]
    ((fn step []
       (when (.find m)
         (cons [[row (.start m) (.end m)]
                (Long/parseLong (.group m))]
           (lazy-seq (step))))))))

(defn find-numbers [lines]
  (map-indexed
    (fn [row line]
      (re-offsets row #"([0-9]+)" line))
    lines))

(defn calc-bounding [[row start end]]                       ; note: end is 1-based.
  (set
    (concat
      ; can be shortened
      [[row (dec start)]                                    ; left
       [row end]                                            ; right
       [(dec row) (dec start)]                              ; top-left
       [(dec row) end]                                      ; top-right
       [(inc row) end]                                      ; bottom-right
       [(inc row) (dec start)]]                             ; bottom-left
      (for [x (range start end)]                            ; above
        [(dec row) x])
      (for [x (range start end)]                            ; below
        [(inc row) x]))))

(defn part1 [path]
  (let [grid    (parse-1 path)
        symbols (find-symbols grid)
        nums    (apply concat (remove nil? (find-numbers grid)))]
    (->> (for [[pts n] nums
               :let [bounds     (calc-bounding pts)
                     intersects (clojure.set/intersection symbols bounds)]
               :when (seq intersects)]
           [n intersects])
      (map first)
      (reduce +))))

(defn part2 [path]                                          ;; slow. not great.
  (let [grid  (parse-1 path)
        gears (find-gears grid)
        nums  (apply concat (remove nil? (find-numbers grid)))]
    (->> (for [gear gears
               [pos n] nums
               :let [bounds (calc-bounding pos)]
               :when (get bounds gear)]
           [gear n])
      (group-by first)
      (filter (fn [[k vs]] (= 2 (count vs))))
      (map (fn [[k vs]] (map second vs)))
      (map #(apply * %))
      (reduce +))))

(comment
  (part1 "resources/2023/day03/demo.in")
  (part1 "resources/2023/day03/problem.in")

  (part2 "resources/2023/day03/demo.in")
  (part2 "resources/2023/day03/problem.in"))