(ns advent-of-code.year2022.day06
  (:require [hyperfiddle.rcf :refer (tests)]))

(defn solve [n line]
  (->> (partition n 1 line)
    (keep-indexed
      (fn [idx e]
        (if (= n (count (distinct e)))
          (+ idx n))))
    (first)))

(tests
  (solve 4 "bvwbjplbgvbhsrlpgdmjqwftvncz") := 5
  (solve 4 "nppdvjthqldpwncqszvftbrmjlhg") := 6
  (solve 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") := 10
  (solve 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") := 11

  (solve 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb") := 19
  (solve 14 "bvwbjplbgvbhsrlpgdmjqwftvncz") := 23
  (solve 14 "nppdvjthqldpwncqszvftbrmjlhg") := 23
  (solve 14 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") := 29
  (solve 14 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") := 26)