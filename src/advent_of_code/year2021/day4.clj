(ns advent-of-code.year2021.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.rpl.specter :as S]))

(defn parse-lines [lines]
  (->> lines
    (S/transform [S/ALL] #(re-seq #"\d+" %))
    (S/transform [S/ALL S/ALL] #(Long/parseLong %))))

(defn build-board [rows]
  (into (mapv set rows)
    (map set (apply map vector rows))))

(defn read-input [path]
  (with-open [rdr (io/reader path)]
    (let [[fst-line _ & lines] (doall (line-seq rdr))]
      {:numbers (mapv #(Long/parseLong %) (clojure.string/split fst-line #","))
       :boards  (mapv (comp build-board parse-lines) (partition 5 6 lines))})))

(defn next-state
  "Removes num from all sets and returns them unless any set is emptied,
  in which case returns the (reduced) product of num and he sum of all distinct remaining numbers."
  [sets num]
  (let [sets' (map #(disj % num) sets)]
    (if (some empty? sets')
      (reduced (* num (reduce + (set (apply concat sets')))))
      sets')))

(defn solve [path]
  (let [{:as input :keys [boards numbers]} (read-input path)]
    (reduce (fn [boards num]
              (let [states' (map #(next-state % num) boards)
                    done    (filter reduced? states')]
                (if (seq done)
                  (reduced @(first done))
                  states')))
      boards numbers)))

(defn solve-part2 [path]
  (let [{:as input :keys [boards numbers]} (read-input path)]
    (->> numbers
      (reduce
        (fn [[winners boards] num]
          (let [boards'  (map #(next-state % num) boards)
                winners' (filter reduced? boards')]
            (if (seq winners')
              [winners' (remove reduced? boards')]
              [winners boards'])))
        [[] boards])
      (ffirst) ;; assumes one winner in last round of wins.
      (deref))))

(comment
  (:numbers (read-input "resources/2021/day4/demo.in"))
  (time (solve "resources/demo.in"))
  (solve "resources/2021/day4/problem.in")
  (time (solve-part2 "resources/problem.in")))