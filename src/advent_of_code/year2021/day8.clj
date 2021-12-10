(ns advent-of-code.year2021.day8
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(def digits
  {0 #{\a \b \c \e \f \g}
   1 #{\c \f}
   2 #{\a \c \d \e \g}
   3 #{\a \c \d \f \g}
   4 #{\b \c \d \f}
   5 #{\a \b \d \f \g}
   6 #{\a \b \d \e \f \g}
   7 #{\a \c \f}
   8 #{\a \b \c \d \e \f \g}
   9 #{\a \b \c \d \f \g}})

(defn solve [line]
  (let [freqs      (frequencies (apply concat (vals digits)))
        idx        (into {} (for [[d vs] digits] [(reduce + (map freqs vs)) d]))
        [in out] (string/split line #" \| ")
        insets     (map set (string/split in #" "))
        outsets    (map set (string/split out #" "))
        infreqs    (frequencies (apply concat insets))
        set->digit (into {} (for [st insets]
                              [st (get idx (reduce + (map infreqs st)))]))]
    (map set->digit outsets)))

(defn part1 [path]
  (with-open [rdr (io/reader path)]
    (reduce + (for [line (line-seq rdr)]
                (->> (solve line)
                  (map #{1 4 7 8})
                  (remove nil?)
                  (count))))))

(defn part2 [path]
  (with-open [rdr (io/reader path)]
    (reduce + (for [line (line-seq rdr)]
                (Long/parseLong (apply str (solve line)))))))

(comment
  (part1 "resources/2021/day8/demo.in")
  (part1 "resources/2021/day8/problem.in")

  (part2 "resources/2021/day8/demo.in")
  (part2 "resources/2021/day8/problem.in"))
