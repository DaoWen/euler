(ns project-euler.problems-030
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 021

(defn euler-021
  "Evaluate the sum of all the amicable numbers under 10000."
  ([] (euler-021 10000))
  ([n] (let [ds (->> (range 1 n)
                     (map #(filter (partial divides? %) (range 1 %)))
                     (map #(reduce + %))
                     (map vector (range 1 n))
                     (into #{}))
             dm (into {} ds)
             pairs (filter (fn [[a b]] (and (not= a b) (ds [b a]))) ds)]
        (reduce + (map first pairs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 022

(defn alpha-val [w] (reduce + (map #(- (int %) 64) w)))

(defn euler-022
  "What is the total of all the name scores in the file?"
  ([] (euler-022 (slurp "data/names.txt")))
  ([names] (->> names (re-seq #"\w+") (sort) (map alpha-val)
                (map * (naturals)) (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 023

(defn euler-023
  "Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers."
  [] (let [x-range   (range 1 28124)
           abundants (set (filter #(> (reduce + (divisors %)) %) x-range))]
       (->> x-range
            (filter (fn [x] (not-any? #(abundants (- x %)) abundants)))
            (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 024

; Not needed -- we can use math.combinatorics
#_(defn permute [digits]
  (if (empty? digits) [()]
    (for [d digits, tail (permute (disj digits d))]
      (cons d tail))))

(defn euler-024
  "What is the millionth lexicographic permutation of the digits 0-9?"
  ([] (euler-024 (apply sorted-set (range 10)) 1000000))
  ([ds n] (->> ds permutations (drop (dec n)) first (apply str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 025

(defn euler-025
  "What is the first term in the Fibonacci sequence to contain 1000 digits?"
  ([] (euler-025 (.pow (biginteger 10) 999)))
  ([n] (->> (fib) (map vector (naturals)) (drop-while #(> n (second %))) ffirst)))

