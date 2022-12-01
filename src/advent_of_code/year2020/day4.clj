(ns advent-of-code.year2020.day4
  (:require [advent-of-code.year2021.utils :as u]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]))

(defn parse-line [line]
  ;(re-seq #"((k):(v))+")
  (into {} (for [kv (string/split line #" ")
                 :let [[k v] (string/split kv #"\:")]]
             [(keyword k) v])))

(defn valid? [passport]
  (clojure.set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} (set (keys passport))))

(clojure.set/intersection #{"byr" "yir" "eyr" "hgt" "hcl" "ecl" "pid"})

(clojure.set/subset? #{:a :b} #{:a :b :c})

(s/valid? (s/and
            (s/+ #"\d+")
            (s/conformer #(Long/parseLong %))
            #(> % 10)) "20")

(s/def ::digit #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(s/valid? (s/+ ::digit) "009")

(s/& #"\d" "009")

(s/valid? (s/or
            :cm (s/and #(re-matches #"\d+cm" %)
                  (s/conformer #(drop-last 2 %)))
            :in #(re-matches #"\d+in" %))
  "456in")

(s/conform (s/or
             :cm (s/and #(re-matches #"\d+cm" %)
                   (s/conformer (comp #(Long/parseLong %) #(apply str %) #(drop-last 2 %)))
                   #(<= 150 % 193))
             :in (s/and #(re-matches #"\d+in" %)
                   (s/conformer (comp #(Long/parseLong %) #(apply str %) #(drop-last 2 %)))
                   #(<= 59 % 76)))
  "151cm")

(re-matches #"\d+cm" %)

(s/valid? (s/+ #"\d") "123")

(s/valid? (re-seq #"\d" "1232") "123")

(s/valid? (s/& #"\d") "123")

(s/valid? (s/and
            (s/+ ::digit)
            (s/conformer #(Long/parseLong %))
            #(> % 10))
  "20")

(take-while (complement #{1}) (iterate (fn [x]
                                         (if (even? x)
                                           (/ x 2)
                                           (inc (* 3 x)))) 5))

;(string/split line #" ")
(valid? (parse-line "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"))
(valid? (parse-line "byr:1937 iyr:2017 cid:147 hgt:183cm"))
(valid? (parse-line "byr:1937 iyr:2017 cid:147 hgt:183cm"))


(s/def ::byr (s/and (s/conformer #(Long/parseLong %)) #(<= 1920 % 2002)))
(s/def ::iyr (s/and (s/conformer #(Long/parseLong %)) #(<= 2010 % 2020)))
(s/def ::eyr (s/and (s/conformer #(Long/parseLong %)) #(<= 2020 % 2030)))
(s/def ::hgt
  (s/or
    :cm (s/and #(re-matches #"\d+cm" %)
          (s/conformer (comp #(Long/parseLong %) #(apply str %) #(drop-last 2 %)))
          #(<= 150 % 193))
    :in (s/and #(re-matches #"\d+in" %)
          (s/conformer (comp #(Long/parseLong %) #(apply str %) #(drop-last 2 %)))
          #(<= 59 % 76))))

(s/or :cm #(string/ends-with? % "cm"))

;(s/int-in-range?)

(s/valid? ::byr "2002")
(s/valid? ::byr "1919")

(s/def ::passport
  (s/keys :req-un ["byr"]))

(clojure.set/intersection
  #{"byr" "yir" "eyr" "hgt" "hcl" "ecl" "pid"}
  (set (keys (parse-line "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"))))

(clojure.set/intersection
  #{"byr" "yir" "eyr" "hgt" "hcl" "ecl" "pid"}
  (set (keys (parse-line "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd byr:1937 iyr:2017 cid:147 hgt:183cm"))))

(u/with-reader [rdr "resources/2020/day4/demo.in"]
  ;(map (comp valid? parse-line))
  (count (filter true? (map valid? (map parse-line (map #(string/join " " %) (remove #{'("")} (partition-by #(string/blank? %) (into [] (line-seq rdr))))))))))

(u/with-reader [rdr "resources/2020/day4/problem.in"]
  ;(map (comp valid? parse-line))
  (count (filter true? (map valid? (map parse-line (map #(string/join " " %) (remove #{'("")} (partition-by #(string/blank? %) (into [] (line-seq rdr))))))))))

