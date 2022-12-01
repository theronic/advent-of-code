(ns advent-of-code.year2021.utils
  (:require [clojure.java.io :as io])
  (:import (java.util InputMismatchException NoSuchElementException)))

(defn scanner [in] (java.util.Scanner. in))

(defmacro with-reader [[sym x] & body]
  `(with-open [~sym (io/reader ~x)]
     ~@body))

(defn sum [coll] (reduce + coll))

(comment
  (macroexpand '(with-open [rdr (io/reader "abc")]
                  rdr))

  (macroexpand '(with-reader [rdr "abc"]
                  (do-thing rdr))))

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