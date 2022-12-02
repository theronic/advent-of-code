(ns advent-of-code.year2022.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [advent-of-code.year2022.utils :as u :refer (sum)]))

(def moves [:rock :paper :scissors])
(def beats? #{[:paper :rock] [:rock :scissors] [:scissors :paper]})
(def offsets {"X" 2, "Y" 0, "Z" 1})

(defn ->move [ch]
  (case ch
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(defn score-round [[their-move my-move]]
  (+ (inc (.indexOf moves my-move))
    (cond
      (beats? [my-move their-move]) 6
      (= my-move their-move) 3
      :else 0)))

(defn parse [path]
  (with-open [rdr (io/reader path)]
    (->> (line-seq rdr)
      (mapv #(string/split % #" ")))))

(defn part1 [path]
  (->> (parse path)
    (map #(map ->move %))
    (map score-round)
    (reduce +)))

(defn adjust-move [[their-symbol my-hint]]
  (let [their-move' (->move their-symbol)
        their-idx   (.indexOf moves their-move')
        my-idx      (+ their-idx (get offsets my-hint))]
    [their-move' (nth (cycle moves) my-idx)]))

(defn part2 [path]
  (->> (parse path)
    (map adjust-move)
    (map score-round)
    (sum)))

(comment
  (part1 "resources/2022/day02/demo.in")
  (part2 "resources/2022/day02/demo.in")

  (part1 "resources/2022/day02/problem.in")
  (part2 "resources/2022/day02/problem.in"))