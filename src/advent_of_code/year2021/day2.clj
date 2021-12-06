(ns advent-of-code.year2021.day2
  (:require [clojure.java.io :as io]
            [advent-of-code.year2021.utils :as u]))

(defn part1-next-state [state [dir size]]
  (case dir
    up (update state :depth #(- % size))
    down (update state :depth #(+ % size))
    forward (update state :horiz #(+ % size))))

(defn line->move [s]
  (if-let [[_ dir size] (re-find #"(forward|down|up) (\d+)" s)]
    [(symbol dir) (Long/parseLong size)]
    (throw (Exception. "unexpected"))))

(defn part1 [lines]
  (let [state (->> (map line->move lines)
                (reduce part1-next-state {:depth 0 :horiz 0}))]
    (* (:depth state) (:horiz state))))

(defn part2-next-state [{:as state :keys [aim horiz depth]} [dir x]]
  (case dir
    up (update state :aim #(- % x))
    down (update state :aim #(+ % x))
    forward {:aim   aim
             :horiz (+ horiz x)
             :depth (+ depth (* aim x))}))

(defn part2 [lines]
  (let [state (->> (map line->move lines)
                (reduce part2-next-state {:aim 0 :depth 0 :horiz 0}))]
    (* (:depth state) (:horiz state))))

(comment
  (with-open [rdr (io/reader "resources/2021/day2/demo.in")]
    (part1 (line-seq rdr)))

  (with-open [rdr (io/reader "resources/2021/day2/demo.in")]
    (part2 (line-seq rdr)))

  (with-open [rdr (io/reader "resources/2021/day2/problem.in")]
    (part1 (line-seq rdr)))

  (with-open [rdr (io/reader "resources/2021/day2/problem.in")]
    (part2 (line-seq rdr))))