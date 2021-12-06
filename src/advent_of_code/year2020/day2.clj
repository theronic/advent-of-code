(ns advent-of-code.year2020.day2
  (:require [clojure.string :as string]))

(def day2-input "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc")

(defn parse-line [line]
  (let [[pattern pass] (string/split line #": ")]
    (if-let [[_ min max letter] (re-find #"(\d+)-(\d+) (.)" pattern)]
      {:min (Long/parseLong min)
       :max (Long/parseLong max)
       :char (first letter)
       :pass pass})))

(defn valid-pass? [{:as policy :keys [min max char pass]}]
  (<= min (get (frequencies pass) char 0) max))

(defn part1 [in]
  (->> in
    (string/split-lines)
    ;(remove (string/blank?))
    (map parse-line)
    (map valid-pass?)
    (filter identity)
    (count)))

(defn valid-pass2? [{:as policy :keys [min max char pass]}]
  ;; what if too long?
  (let [p1? (= char (get pass (dec min)))
        p2? (= char (get pass (dec max)))]
    (or (and p1? (not p2?))
      (and p2? (not p1?)))))

(defn part2 [in]
  (->> in
    (string/split-lines)
    (map parse-line)
    (map valid-pass2?)
    (filter identity)
    (count)))

(comment
  (part1 (slurp "resources/2020/day2/demo.in"))
  (part1 (slurp "resources/2020/day2/problem.in"))
  (part2 (slurp "resources/2020/day2/demo.in"))
  (part2 (slurp "resources/2020/day2/problem.in")))