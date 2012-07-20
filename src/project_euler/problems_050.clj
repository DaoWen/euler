(ns project-euler.problems-050
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 051

(defn euler-051 "What is the largest n-digit pandigital prime that exists?"
  ([] (euler-051 9))
  ([n] (->> (for [i (range 2 (inc n))]
              (->> (range 1 i)
                   permutations
                   (map seq2num)
                   (filter #(.isProbablePrime (biginteger %) 15))
                   (reduce max 0)))
            (reduce max))))
(time (euler-051))

