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

(defn pent-num "Return the nth pentagonal number."
  [n] (quot (* n (dec (* 3 n))) 2))

(defn pent-inv "Return the i for some pentagonal number P(i)."
  [n] (-> n (* 24) inc Math/sqrt long inc (quot 6)))

(defn pent-num? "Checks if the given integer is a pentagonal number."
  [n] (zero? (-> n (* 24) inc Math/sqrt inc (/ 6) (rem 1))))

(defn pent-diff "Difference between P(j) and P(i), with j defaulting to i+1."
  ([i] (inc (* 3 i)))
  ([i j] (quot (* (- j i) (dec (* 3 (+ j i)))) 2)))

(defn summ "Summation of x*i for i=1 thru n"
  [n x] (* x (/ (* n (inc n)) 2)))

(defn pent-triples
  "Lazy sequence of all [x y] pairs such that P(i-x) P(i) P(i+y) are equidistant.
   Returns [x y i d] where d is the distance of P(i) to each P(i-x) and P(i+y)."
  [] (for [x (iterate inc 2), y (range 1 x)
           :let [n (dec x), sn (summ n 3)
                 m (+ n y), sm (summ m 3)
                 d (/ (- sm (* 2 sn)) (- x y))]
           :when (and (> d 4) (= 1 (rem d 3)))
           :let [i (/ (dec d) 3)]]
       [x y i (pent-diff i (+ i x))]))

(defn euler-044
  "Find the pentagonal numbers such that P(i)+P(j) and P(i)-P(j) are also pentagonal.
   Returns [P(i) P(j) P(i)-P(j)]"
  [] (let [dist      #(% 3)
           pentf     (comp pent-num? dist) 
           terms     (drop-while (complement pentf) (pent-triples))
           ceil      (-> terms first dist pent-inv inc)
           terms'    (->> terms (take-while #(<= (first %) ceil)) (filter pentf))
           [x y i d] (reduce (partial min-key dist) terms')]
       [(pent-num (+ x i)) d (pent-num i)]))

(defn closed-pent-pairs
  "Lazy sequence of pentagonal number pairs such that P(i)+P(j) and P(i)-P(j) are also pentagonal.
   Pairs are orderd from smallest to largest on P(i) then P(j). Returns [i P(i) j P(j)]"
  [] (let [pents (map #(-> [% (pent-num %)]) (iterate inc 1))]
       (for [[i a] pents, [j b] (take-while #(< (second %) a) pents)
             :when (and (pent-num? (+ a b)) (pent-num? (- a b)))] [i a j b])))

(defn euler-044-slower
  "Find the pentagonal numbers such that P(i)+P(j) and P(i)-P(j) are also pentagonal.
   Returns [P(i) P(j) P(i)-P(j)]"
  [] (let [[j a _ b] (first (closed-pent-pairs))
           low-bound (- j (pent-inv (- a b)))
           possibles (take-while #(<= (first %) low-bound) (pent-triples))
           min-4th   (partial min-key #(% 3))
           [x y i d] (->> possibles (filter #(pent-num? (% 3))) (reduce min-4th))]
       [a b (- a b)]))

