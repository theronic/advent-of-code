(ns advent-of-code.year2023.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (map (fn [line]
             (let [[card winning have] (map string/trim (string/split line #":|\|"))]
               [(Long/parseLong (second (re-find #"Card +(\d+)" card)))
                (set (map parse-long (string/split winning #" +")))
                (set (map parse-long (string/split have #" +")))])))
      (doall))))

(defn score [vs]
  (reduce (fn [acc v]
            (if (zero? acc) 1 (* acc 2)))
    0 vs))

(defn part1 [path]
  (->> (parse path)
    (map (fn [[card winning have]]
           (clojure.set/intersection winning have)))
    (map score)
    (reduce +)))

(defn part2 [path]
  (let [cards (parse path)]
    (->> cards
      (map (fn [[card winning have]]
             [card (count (clojure.set/intersection winning have))]))
      (reduce (fn [acc [card cnt]])
        {})
      (map score)
      (reduce +))))

(defn solve2 [cards [card-id winning have :as card]]
  ; repeat card N times for winning count
  (let [cnt (count (clojure.set/intersection winning have))]
    (inc (count (take cnt cards)))))

(comment
  (let [cards (parse "resources/2023/day04/demo.in")]
    ;(map (fn [[card winning have]]
    ;       [card (count (clojure.set/intersection winning have))]))
    (reduce
      (fn [acc [id winning have :as card]]
        (let [[fst & rst] cards
              prev-cnt (count acc)                          ; can we do this with a counter plz?
              cnt      (count (clojure.set/intersection winning have))]
          (concat)

          (into (conj acc card) (take cnt (drop prev-cnt cards)))))
      cards
      cards))
    ;(map #(solve2 cards %))
    ;(reduce +))
      ;(reduce (fn [acc [card cnt]])
      ;  {})
      ;(map score)
      ;(reduce +)))

  (part1 "resources/2023/day04/demo.in")
  (part1 "resources/2023/day04/problem.in")

  (part2 "resources/2023/day03/demo.in")
  (part2 "resources/2023/day03/problem.in"))