(ns advent-of-code.year2021.day16
  (:use [gloss.core])
  (:require [clojure.java.io :as io])
  (:import java.util.BitSet))

;; Unsolved.

;(defn string->bitset [s]
;  (let [bs (BitSet. (count s))]
;    (doall (map-indexed (fn [idx v]
;                          (.set bs idx ({\0 false \1 true} v)))
;             s))
;    bs))

(defn hex->binary-string [s]
  (.toString (BigInteger. s 16) 2))

(comment
  (with-open [rdr (io/reader "resources/2021/day16/demo.in")]
    (map hex->binary-string (into [] (line-seq rdr)))))

(defn parse-literal [bits]
  (BigInteger. (->> (partition 5 bits)
                 (mapcat rest)
                 (apply str)) 2))

(comment
  (first (drop 5 (range 5)))

  (BigInteger. (apply str '(\1 \1 \0)) 2)

  (into [] cat (take-while (comp #{1} first) (partition-all 5 [1 0 0 0 0, 1 0 0 0 0, 0 1 1])))
  (drop-while (comp #{1} first) (partition-all 5 [1 0 0 0 0, 1 0 0 0 0, 0 1 1]))

  (inc (count (take-while #{\1} (take-nth 5 [0 0 0 0 0])))))

;(defn parse-literal [bits]
;  (loop [bits bits val ()]
;    (let [[[pre & grp] bits] (split-at 5 bits)
;          val (concat val grp)]
;      (if (= \0 pre)
;        [{:val (num<-bits val)} bits]
;        (recur bits val)))))

;(into [] cat (take-while (comp #{1} first) (partition-all 5 [])))

;(mapcat rest [[1 1 1] [0 1 1]])

(let [bits [\1 \0 \1 \0 \1,
            \1 \0 \0 \1 \0,
            \1 \0 \1 \0 \1,
            \0 \0 \1 \1 \1,
            \0 \0 \0]

      cnt  (inc (count (take-while #{\1} (take-nth 5 bits))))]
  ;(take (* 5 cnt) bits)
  (mapcat rest (take cnt (partition 5 bits))))

(defn parse [num-packets bits]
  (prn "num-packets" num-packets "cnt" (count bits) bits)
  (if (and (number? num-packets) (zero? num-packets))
    [:done bits]
    (when (and (seq bits) (first (drop 5 bits)))
      (let [[vvv bits] (split-at 3 bits)
            [ttt bits] (split-at 3 bits)
            version     (BigInteger. (apply str vvv) 2)
            packet-type (BigInteger. (apply str ttt) 2)]
        (prn "version" version "type" packet-type)
        (case packet-type
          4 (let [num-segments (inc (count (take-while #{\1} (take-nth 5 bits))))
                  grp (partition 5 bits)
                  literal-bits (mapcat rest (take num-segments grp)) ; into [] cat (take-while (comp #{1} first) (partition-all 5 bits))) ;; wrong
                  literal      (parse-literal literal-bits)
                  not-literal  (drop (* 5 num-segments) bits)
                  _            (prn "literal bits" literal-bits)]
              (prn "literal val" (parse-literal literal-bits))
              (cons
                [{:type    packet-type
                  :version version
                  :value   literal
                  :rem     (count bits)} not-literal])
              (lazy-seq nil))
                  ;(parse (if num-packets (dec num-packets)) (drop (count literal-bits) bits)))))
          (let [[len-type & bits] bits]
            (prn "len-type" len-type)
            (case len-type
              \0 (let [[len-bits bits] (split-at 15 bits)
                       num-bits (BigInteger. (apply str len-bits) 2)]
                   (prn "num-bits" num-bits)
                   (cons
                     [{:type packet-type :version version :num-bits num-bits :rem (count bits)}
                      (drop num-bits bits)]
                     (lazy-seq                              ;; what if there are bits left over?
                       (parse nil (take num-bits bits)))))
                       ;(if (nil? num-packets)
                       ;  (parse nil (take num-bits bits))
                       ;  (parse nil (take num-bits bits))))))
              \1 (let [[num-ps bits] (split-at 11 bits)
                       num-subpackets (BigInteger. (apply str num-ps) 2)]
                   (prn "num subpackets:" num-subpackets)
                   (cons {:type packet-type :version version :num-sub num-subpackets :rem (count bits)}
                     (lazy-seq (parse num-subpackets bits)))))))))))

(comment
  (parse nil [1 1 1 0 0 0 1 1 1])
  (count (take-while #{\1} (take-nth 5 "101111111000101"))))

(comment
  ; 4 1 5 6
  (take 2 (parse 1 (hex->binary-string "620080001611562C8802118E34")))
  (take 2 (parse 1 (hex->binary-string "C0015000016115A2E0802F182340")))

  (take 3 (parse nil (hex->binary-string "8A004A801A8002F478")))
  (take 4 (parse nil (hex->binary-string "8A004A801A8002F478")))

  (reduce + (map :version (take 4 (parse 1 (hex->binary-string "8A004A801A8002F478"))))) ;; werk
  (parse 1 (hex->binary-string "EE00D40C823060"))
  (take 100 (parse 1 (hex->binary-string "8A004A801A8002F478")))
  (parse 1 (hex->binary-string "D2FE28"))
  (reduce + (map :version (parse 1 (hex->binary-string "620080001611562C8802118E34"))))
  (parse 1 (hex->binary-string "A0016C880162017C3686B18A3D4780"))
  "8A004A801A8002F478"

  (parse 1 (hex->binary-string "A0016C880162017C3686B18A3D4780"))
  (process-packet-stream (mapcat hex-to-binary "A0016C880162017C3686B18A3D4780"))

;(seq (string->bitset "110001"))

;; VVVTTTAAAAABBBBBCCCCC

  (defcodec type (enum :byte :rectangle :triangle :circle))

  (defcodec triangle {:type :triangle, :width :int32, :height :int32})
  (defcodec rectangle {:type :rectangle, :width :int32, :height :int32})
  (defcodec circle {:type :circle, :radius :int32})

  (defcodec codec
    (header
      type
      {:triangle triangle, :rectangle rectangle, :circle circle}
      :type))

  (defcodec)

  (with-open [rdr (io/reader "resources/2021/day16/demo.in")]
    (into [] (line-seq rdr)))

  (g/compile-frame))
