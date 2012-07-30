(ns project-euler.problems-060
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations combinations)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 051

(defn euler-051
  "Find the smallest prime which, by replacing part of the number (not necessarily
   adjacent digits) with the same digit, is part of an eight prime value family."
  ([] (euler-051 8))
  ([l] (let [primes (->> (gen-primes) (drop-while #(< % 100)))]
         (first 
           (for [digit-count (iterate inc 3)
                 :let [top (Math/pow 10 (inc digit-count))
                       bot (quot top 10)
                       ps (->> primes (drop-while #(< % bot)) (take-while #(< % top)))
                       ss (->> ps (map (comp vec num2seq)) (filter #(not= % (distinct %))))]
                 [i j] (combinations (range 0 digit-count) 2)
                 :let [dropf  #(when-not (#{i j} %) %2)
                       ss' (filter #(= (% i) (% j)) ss)
                       groups (group-by #(keep-indexed dropf %) ss')]
                 group (map (fn [[_ x]] [(seq2num (first x)) (count x) i j]) groups)
                 :when (= l (second group))]
             group)))))

(time (euler-051 7))

