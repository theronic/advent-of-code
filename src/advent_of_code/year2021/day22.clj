(ns advent-of-code.year2021.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.util BitSet)))

;; Unsolved.

(defrecord Cube [x1 x2, y1 y2, z1 z2])

(defn cut [a1 a2, b1 b2]
  (i))

(defn intersect
  [& cubes]
  (map :x1 cubes))

;; algo: read instructions in reverse.
;; maintain cube ranges that are either on or off.
;; for every new instruction, intersect.


(defn parse-line [line]
  (let [[set? & nums] (rest (re-find #"(on|off) x=(\d+)\.\.(\d+),y=(\d+)\.\.(\d+),z=(\d+)\.\.(\d+)" line))]
    (into [({"on" true "off" false} set?)] (map #(Long/parseLong %) nums))))
  ;(let [[_ x1 x2, y1 y2, z1 z2]
  ;      (re-find #"(on|off) x=(\d+)\.\.(\d+),y=(\d+)\.\.(\d+),z=(\d+)\.\.(\d+)")]))

(def *cmds (with-open [rdr (io/reader "resources/2021/day22/demo.in")]
             (map parse-line (into [] (line-seq rdr)))))

(->> *cmds
  (mapv rest)
  (map #(partition 2 %)))

(into [] (comp
           (map rest)
           (partition-all 2)) *cmds)

(BitSet. (* 100000 100000 100000))

(frequencies (map even? (for [z (range -50000 (inc 50000))
                              y (range -50000 (inc 50000))
                              x (range -50000 (inc 50000))]
                          (+ x y z))))

(loop [x 0]
  (if (>= x 1000000000000000)
    x
    (recur (inc x))))

(* 100000 100000 100000)