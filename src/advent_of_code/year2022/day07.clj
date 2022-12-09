(ns advent-of-code.year2022.day07
  (:require [hyperfiddle.rcf :refer (tests)]
            [com.rpl.specter :as S]
            [clojure.string :as string]))

(defn load-tree [path]
  (->> (string/split-lines (slurp path))
    (reduce
      (fn [{:as state :keys [path tree]} line]
        (if (string/starts-with? line "$")
          (if-some [[_ sub-path] (re-find #"\$ cd (.+)" line)]
            (case sub-path
              "/" (S/setval [:path] ["/"] state)
              ".." (S/setval [:path S/LAST] S/NONE state)
              (S/setval [:path S/END] [sub-path] state))
            state)                                          ;; do nothing for $ ls or unhandled commands.
          (let [[_ size-or-dir filename] (re-find #"(dir|\d+) (.+)" line)]
            (if (= "dir" size-or-dir)
              state                                         ;; dir. we don't mark specially.
              (let [size  (Integer/parseInt size-or-dir)
                    tree' (S/setval [path filename] size tree)]
                (assoc state :tree tree'))))))
      {:path ["/"] :tree {}})
    (:tree)))

(defn tree-sum [node]
  (if (map? node)
    (reduce + (map tree-sum (vals node)))
    node))

(defn part1 [path]
  (->> (load-tree path)
    (tree-seq map? vals)
    (remove number?)                                        ;; remove files.
    (map tree-sum)
    (filter #(<= % 100000))
    (reduce +)))

(defn part2 [path]
  (let [disk-size  70000000
        needed     30000000
        tree       (load-tree path)
        total-size (tree-sum tree)
        free       (- disk-size total-size)
        short      (- needed free)]
    (->> (tree-seq map? vals tree)
      (remove number?)
      (map tree-sum)
      (sort)
      (filter #(>= % short))
      (first))))

(tests
  (part1 "resources/2022/day07/demo.in") := 95437
  (part1 "resources/2022/day07/problem.in")

  (part2 "resources/2022/day07/demo.in") := 24933642
  (part2 "resources/2022/day07/problem.in"))