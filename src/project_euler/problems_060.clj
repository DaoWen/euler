(ns project-euler.problems-060
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations combinations)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 051

(defn euler-051
  "Find the smallest prime which, by replacing part of the number (not necessarily
   adjacent digits) with the same digit, is part of an eight prime value family."
  ([] (euler-051 8))
  ([l] (let [starts (set (range 0 (- 11 l)))
             primes (->> (gen-primes) (drop-while #(< % 10)))]
         (first 
           (for [s (map (comp vec num2seq) primes)
                 :let [digit-count (count s)]
                 stars   (range 1 digit-count)
                 indices (combinations (range 0 digit-count) stars)
                 :let [i (first indices)]
                 :when (and (contains? starts (s i))
                            (apply = (map #(s %) indices)))
                 :let [base     (seq2num (apply assoc s (mapcat list indices (repeat 0))))
                       powers   (map #(long (Math/pow 10 (- digit-count 1 %))) indices)
                       s-nums   (map #(apply + base (map (partial * %) powers)) (range 1 10))
                       s-group  (if (= i 0) s-nums (cons base s-nums))
                       s-primes (filter prime? s-group)]
                 :when (<= l (count s-primes))]
             [(first s-group) s-primes])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 052

(defn euler-052
  "Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits."
  ([] (euler-052 6))
  ([n] (first
         (for [x (iterate inc 1)
               :let [xs (map #(-> % (* x) str sort) (range 1 (inc n)))]
               :when (apply = xs)] x))))

