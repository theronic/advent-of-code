(ns advent-of-code.year2022.day06)

(defn solve [n line]
  (->> (partition n 1 line)
    (keep-indexed
      (fn [idx e]
        (if (= n (count (distinct e)))
          (+ idx n))))
    (first)))

(comment
  (solve 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
  (solve 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"))