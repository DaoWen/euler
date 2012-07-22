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
  [t] (zero? (-> t (* 8) inc Math/sqrt inc (/ 2) (rem 1))))

(defn euler-042 "How many are triangle words are in the given string?"
  ([] (euler-042 (re-seq #"\w+" (slurp "data/p042.txt"))))
  ([words] (reduce + (for [w words
                           :let [v (reduce + (map #(- (int %) 64) w))]
                           :when (tri-num? v)] 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 043

(defn euler-043
  ([] (euler-043 (range 10) 3 [1 2 3 5 7 11 13 17]))
  ([ds l xs] (->> (for [dseq (permutations ds)
                        :let [dseq (vec dseq)]
                        :when (and (pos? (dseq 0))
                                   (even? (dseq 3))
                                   (#{0 5} (dseq 5)))
                        :when (->> (partition l 1 dseq)
                                   (map seq2num)
                                   (map vector xs)
                                   (every? (fn [[p d]] (divides? d p))))]
                    (seq2num dseq))
                  (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 044

(defn pent-num? "Checks if the given integer is a pentagonal number."
  [t] (zero? (-> t (* 24) inc Math/sqrt inc (/ 6) (rem 1))))

(defn pentagonals "Lazy sequence of pentagonal numbers" []
  (map #(quot (* % (dec (* 3 %))) 2) (iterate inc 1)))

(defn euler-044 
  "Find the smallest pair of pentagonal numbers such that Pi+Pj and Pi-Pj are also pentagonal."
  [] (first (let [pents (pentagonals)]
              (for [a pents, b (take-while #(< % a) pents)
                    :when (and (pent-num? (+ a b)) (pent-num? (- a b)))] [a b (- a b)]))))

(defn pent-diff [i j]
  (if (> i j) (recur j i)
    (quot (* (- j i) (dec (* 3 (+ j i)))) 2)))

(defn pent-diffs []
  (for [j (naturals), i (range 1 j)
        :let [d (pent-diff i j)]
        :when (pent-num? d)] [d [i j]]))

(defn pent-diffs []
  (for [j (naturals), i (range 1 j)
        :let [d (pent-diff i j)]
        :when (pent-num? d)
        k (range (inc j) (- (+ j j) i))
        :when (= d (pent-diff j k))]
    [d [i j k]]))

(defn summ "Summation of x*i for i=1 thru n" [n x]
  (* x (/ (* n (inc n)) 2)))

(defn pentagonal-triple-ranges
  "Lazy sequence of all [x y] pairs such that P(i-x) P(i) P(i+y) are equidistant."
  [] (for [x (iterate inc 2), y (range 1 x)
           :let [n (dec x), sn (summ n 3)
                 m (+ n y), sm (summ m 3)
                 d (/ (- sm (* 2 sn)) (- x y))]
           :when (and (> d 4) (= 1 (rem d 3)))
           :let [i (/ (dec d) 3)]]
       [x y i (pent-diff i (+ i x)) (pent-diff (+ i x) (+ i x y))]))

(time (->> (pentagonal-triple-ranges)
           (filter (fn [[x y i d _]] (pent-num? d)))
           (take 2)))

(defn pent-i [n]
  (some #(if (<= n (second %)) %) (map vector (naturals) (pentagonals))))

(defn third [xs] (second (rest xs)))

(time (->> (pentagonal-triple-ranges) (take-while (fn [[_ _ i]] (< i 2167))) (reduce (partial max-key #(% 2)))))

(let [n 3
      ps (cons 0 (pentagonals))
      [x y i] (nth (pentagonal-triple-ranges) n)
      [a b c] [(nth ps i) (nth ps (+ i x)) (nth ps (+ i x y))]]
  [a b c (= (- b a) (- c b))])

