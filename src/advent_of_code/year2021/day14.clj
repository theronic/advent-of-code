(ns advent-of-code.year2021.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn parse-rule [line]
  (let [[_ pair [v]] (re-find #"(.+) -> (.+)" line)
        [a b] pair]
    [[a b] [[a v] [v b]]]))

(defn apply-rules [rules init]
  (reduce (fn [acc [pair cnt]]
            (if-let [new (get rules pair)]
              (if (pos? cnt)
                (merge-with +
                  (update acc pair - cnt)
                  (into {} (map (fn [x] [x cnt]) new)))
                acc)
              acc))
    init init))

(defn solve [n path]
  (with-open [rdr (io/reader path)]
    (let [[template _ & more] (doall (line-seq rdr))
          rules   (into {} (map parse-rule more))
          init    (frequencies (conj (map vec (partition 2 1 template)) [(last template)]))]
      (->> (nth (iterate (partial apply-rules rules) init) n)
        (map (fn [[ks cnt]] {(first ks) cnt}))
        (apply merge-with +)
        (vals)
        (apply (juxt max min))
        (apply -)))))

(defn part1 [path] (solve 10 path))

(defn part2 [path] (solve 40 path))

(comment
  (part1 "resources/2021/day14/demo.in")
  (part1 "resources/2021/day14/problem.in")

  (part2 "resources/2021/day14/demo.in")
  (time (part2 "resources/2021/day14/problem.in")))
