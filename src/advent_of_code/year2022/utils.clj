(ns advent-of-code.year2022.utils
  (:require [clojure.java.io :as io])
  (:import (java.util InputMismatchException NoSuchElementException)))

(defn scanner [in] (java.util.Scanner. in))

(defmacro with-reader [[sym x] & body]
  `(with-open [~sym (io/reader ~x)]
     ~@body))

(defn sum [coll] (reduce + coll))

(defn parse-int [s]
  (Integer/parseInt s))

(defn parse-long [s]
  (Long/parseLong s))

(defn int-seq [^java.util.Scanner scanner]
  (try (cons (.nextInt scanner)
         (lazy-seq (int-seq scanner)))
       ;(catch InputMismatchException ex nil)
       (catch NoSuchElementException ex
         nil)))

(defmacro scan-seq [^java.util.Scanner scanner method]
  `(try (cons (~method ~scanner)
          (lazy-seq (int-seq ~scanner)))
        (catch NoSuchElementException ex
          nil)))

(comment
  (macroexpand '(scan-seq 123 .nextInt)))