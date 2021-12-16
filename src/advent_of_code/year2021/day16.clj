(ns advent-of-code.year2021.day16
  (:use [gloss.core])
  (:require [clojure.java.io :as io])
  (:import java.util.BitSet))

(defn string->bitset [s]
  (let [bs (BitSet. (count s))]
    (doall (map-indexed (fn [idx v]
                          (.set bs idx ({\0 false \1 true} v)))
             s))
    bs))

(compile-frame
  (bit-seq 3))

(defn hex->binary-string [s]
  (.toString (BigInteger. s 16) 2))

(with-open [rdr (io/reader "resources/2021/day16/demo.in")]
  (map hex->binary-string (into [] (line-seq rdr))))

(defn parse-literal [bits]
  (BigInteger. (apply str (mapcat rest (partition 5 bits))) 2))

(defn parse-operater [len-type & bits]
  (case len-type
    \0 (let [[len-bits bits] (split-at 15 bits)]
         (take (BigInteger. (apply str len-bits) 2) bits))
    \1 (let [[num-ps bits] (split-at 11 bits)]
         (partition (int (/ (count bits) (BigInteger. num-ps 2))) bits))))

(defn parse [num-packets bits]
  (prn num-packets "rem" (count bits) bits)
  (when (and (seq bits) (pos? num-packets))
    (let [;num-packets (dec num-packets)
          [vvv bits] (split-at 3 bits)
          [ttt bits] (split-at 3 bits)
          version     (BigInteger. (apply str vvv) 2)
          packet-type (BigInteger. (apply str ttt) 2)]
      (prn "v" version "type" packet-type)
      (case packet-type
        4 (let [cnt (inc (count (take-while #{\1} (take-nth 5 bits))))
                [literal-bits bits] (split-at (* 5 cnt) bits)]
            (prn "literal" cnt)
            (into [{:type    packet-type
                    :version version
                    :value   (parse-literal literal-bits)}]
              (parse (dec num-packets) bits)))
        (let [[len-type & bits] bits]
          (prn "len-type" len-type)
          (case len-type
            \0 (let [[len-bits bits] (split-at 15 bits)
                     num-bits (BigInteger. (apply str len-bits) 2)]
                 (into [{:type packet-type :version version}]
                   (parse 1 (take num-bits bits))))
            \1 (let [[num-ps bits] (split-at 11 bits)
                     num-subpackets (BigInteger. (apply str num-ps) 2)]
                 (prn "num subpackets:" num-subpackets)
                 (into
                   [{:type packet-type :version version}]
                   (parse num-subpackets bits)))))))))

(count (take-while #{\1} (take-nth 5 "101111111000101")))

;(take-while)

; 0111 1110 0101

(comment
  (parse 1 (hex->binary-string "D2FE28"))
  (reduce + (map :version (parse 1 (hex->binary-string "620080001611562C8802118E34"))))
  (parse 1 (hex->binary-string "A0016C880162017C3686B18A3D4780"))
  (parse 1 (hex->binary-string "8A004A801A8002F478"))
  8A004A801A8002F478)


(seq (string->bitset "110001"))

(map-indexed (fn [x y] [x y]) (range 10))



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

(g/compile-frame)
