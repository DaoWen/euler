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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 052

(defn tri-num? "Checks if the given integer is a triangle number." [t]
  (and (pos? t) (zero? (rem (/ (- (Math/sqrt (+ 1 (* 8 t))) 1) 2) 1))))

(defn euler-052 "How many are triangle words are in the given string?"
  ([] (euler-052 (re-seq #"\w+" (slurp "data/p052.txt"))))
  ([words] (reduce + (for [w words
                           :let [v (reduce + (map #(- (int %) 64) w))]
                           :when (tri-num? v)] 1))))

