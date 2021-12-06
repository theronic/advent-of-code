(ns advent-of-code.year2021.day3
  (:require [clojure.java.io :as io]
            [advent-of-code.year2021.utils :as u])
  (:import (java.util BitSet)))

(defn line->bitset [line]
  (-> (BigInteger. line 2)
    (.toByteArray)
    (BitSet/valueOf)))

(defn lines->bitsets
  "Least significant bit on left."
  [lines]
  (->> lines
    (map (comp #(apply str %) reverse))
    (map line->bitset)))

(defn bools->bitset [bools]
  (let [len  (count bools)
        bset (BitSet. len)]
    (doseq [n (range len)]
      (.set bset n (get bools n)))
    bset))

(defn transpose-bitsets [bsets]
  (let [cardinality (apply max (map #(.length %) bsets))]
    (for [n (range cardinality)]
      ;; can combine construction here.
      (bools->bitset (mapv #(.get % n) bsets)))))

(defn count-bits [bset]
  (if (zero? (.length bset))
    false
    (> (/ (.cardinality bset) (.length bset)) 1/2)))

(defn bitset->bigint [bitset]
  (reduce (fn [bigint i]
            (if (.get bitset i)
              (.setBit bigint i)
              bigint))
    (BigInteger/ZERO)
    (range (.length bitset))))

(defn invert-bitset [bitset]
  (let [clone (.clone bitset)]
    (.flip clone 0 (.length bitset))
    clone))

(defn bools->bigint [coll]
  (reduce (fn [bigint n]
            (if (get coll n)
              (.setBit bigint n)
              bigint))
    (BigInteger/ZERO)
    (range (count coll))))

(defn part1 [rdr]
  (let [bset (->> (line-seq rdr)
               (lines->bitsets)
               (transpose-bitsets)
               (map count-bits)
               (reverse)                                    ;; because of the initial reverse.
               (vec)
               (bools->bitset))]
    (* (bitset->bigint bset) (bitset->bigint (invert-bitset bset)))))

(defn part2 [rows invert-condition?]
  (let [cols (apply map vector rows)]
    (->> (range (count cols))
      (reduce
        (fn [rows idx]
          (if (next rows)
            (let [cols        (apply map vector rows)
                  freqs       (mapv frequencies cols)
                  {n0s \0 n1s \1} (get freqs idx)
                  condition   (>= n1s n0s)
                  most-common (if (if invert-condition? (not condition) condition)
                                \1 \0)]
              (filter #(= most-common (nth % idx)) rows))
            (reduced rows)))
        rows)
      (first)
      (map {\0 false \1 true})
      (reverse) (vec)
      (bools->bigint))))

(defn solve-part2 [rdr]
  (let [rows (->> (line-seq rdr) (mapv seq))]
    (* (part2 rows false) (part2 rows true))))

(comment

  (u/with-reader [rdr "resources/2021/day3/demo.in"]
    (part1 rdr))

  (u/with-reader [rdr "resources/2021/day3/demo.in"]
    (solve-part2 rdr))

  (with-open [rdr (io/reader "resources/2021/day3/problem.in")]
    (part1 rdr))

  (with-open [rdr (io/reader "resources/2021/day3/problem.in")]
    (solve-part2 rdr))

  (let [scnr (doto (u/scanner (io/reader "resources/2021/day3/demo.in"))
               (.useRadix 2))]
    (.nextInt scnr))

  (time (with-open [rdr (io/reader "resources/2021-day3-part1.txt")]
          (part1 rdr)))

  (with-open [rdr (io/reader "resources/2021-day3-demo.txt")]
    (solve-part2 rdr))

  (time (with-open [rdr (io/reader "resources/2021-day3-part1.txt")]
          (solve-part2 rdr))))