(ns project-euler.problems-050
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 041

(defn euler-041 "What is the largest n-digit pandigital prime that exists?"
  ([] (euler-041 9))
  ([n] (->> (for [i (range 2 (inc n))]
              (->> (range 1 i)
                   permutations
                   (map seq2num)
                   (filter #(.isProbablePrime (biginteger %) 15))
                   (reduce max 0)))
            (reduce max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 042

(defn tri-num? "Checks if the given integer is a triangle number."
  [t] (and (pos? t) (zero? (rem (/ (- (Math/sqrt (+ 1 (* 8 t))) 1) 2) 1))))

(defn euler-042 "How many are triangle words are in the given string?"
  ([] (euler-042 (re-seq #"\w+" (slurp "data/p042.txt"))))
  ([words] (reduce + (for [w words
                           :let [v (reduce + (map #(- (int %) 64) w))]
                           :when (tri-num? v)] 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 043

(defn euler-043
  ([] (euler-043 (range 10) [1 2 3 5 7 11 13 17]))
  ([ds xs] (->> (for [dseq (permutations ds)
                      :let [dseq (vec dseq)]
                      :when (and (pos? (dseq 0))
                                 (even? (dseq 3))
                                 (#{0 5} (dseq 5)))
                      :when (->> (partition 3 1 dseq)
                                 (map seq2num)
                                 (map vector xs)
                                 (every? (fn [[p d]] (divides? d p))))]
                  (seq2num dseq))
                (reduce +))))

