(ns project-euler.problems-050
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations combinations)]))

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
;; Problem 045

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 045

(defn hex-num? "Checks if the given integer is a hexagonal number."
  [n] (== 3 (-> n (* 8) inc Math/sqrt (rem 4))))

(defn euler-045
  "After 40755, what is the next triangle number that is also pentagonal and hexagonal?"
  ([] (euler-045 40755))
  ([n] (->> (-> n (* 8) inc Math/sqrt inc (quot 4) long inc)
            (iterate inc)
            (map #(* % (dec (* 2 %))))
            (filter pent-num?)
            first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 046

(defn euler-046
  "What is the smallest odd composite that cannot be
   written as the sum of a prime and twice a square?"
  [] (let [prime?  #(.isProbablePrime (biginteger %) 10)
           sqrs-2x (map #(* % % 2) (iterate inc 1))]
       (first (for [n (iterate #(+ 2 %) 9)
                    :when (not (prime? n))
                    :let [xs (take-while #(< % n) sqrs-2x)
                          ys (map #(- n %) xs)]
                     :when (not-any? prime? ys)] n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 047

(defn euler-047
  "Find the first four consecutive integers to have four distinct primes factors."
  [] (loop [n 1, i 0]
       (let [pf-count (-> n prime-factors distinct count)]
         (cond (= i 4) (- n 4)
               (= pf-count 4) (recur (inc n) (inc i))
               :else (recur (inc n) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 048

(defn euler-048
  "Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000."
  ([] (euler-048 1000 10))
  ([n l] (let [lim (.pow (biginteger 10) (biginteger l))
               xs  (map (comp #(.modPow % % lim) biginteger) (range 1 (inc n)))
               sum (biginteger (reduce + xs))]
           (.mod sum lim))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 049

(defn euler-049
  "Find arithmetic sequences, made of 3 prime terms, whose four digits are permutations of each other."
  [] (->> (gen-primes)
          (drop-while #(< % 1000))
          (take-while #(< % 10000))
          (group-by (comp set str))
          vals
          (filter #(>= (count %) 3))
          (mapcat #(combinations % 3))
          (filter (fn [xs] (apply = (map #(apply - %) (partition 2 1 xs)))))
          doall))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 391

(defn S-seq []
  ((fn sk [k acc]
     (lazy-seq
       (let [digits (map chr2int (Long/toString k 2))
             ones   (reduce + digits)
             sum    (+ ones acc)]
         (cons [sum ones] (sk (inc k) sum))))) 0 0))

(defn M 
  ([n] (M n n))
  ([n gap]
    (let [ss  (->> (S-seq) (take-while #(<= (second %) gap)) reverse)
          end (ffirst ss)]
      (loop [[[x k] & ss] ss, y 0, i n, mvs 0]
        (cond (zero? x)   {:start y :end end :moves mvs} ; Success--found full move chain
              (< i (- n)) 0 ; Fail--no forcing move
              (neg? i)    (recur ss x (- n k) (inc mvs)) ; Found forcing move
              :else       (recur ss y (- i k) mvs))))))

(def cube #(* % % %))

(defn p391 [x]
  (->>
    (for [n (range 1 (inc x))]
      (cond
        (< n 18) (:start (M n))
        (< n 50) (:start (M n 10))
        (< n 2000) (:start (M n 15))))
    (map cube)
    (reduce +)))

(defn count-on-bits' [n]
  (loop [n (long n), k 0]
    (if (== 0 n) k
      (recur (bit-and n (dec n)) (inc k)))))

; Redefine with precomputed bytes
(let [counts (int-array 256)]
  (doseq [i (range 0 256)]
    (aset-int counts i (count-on-bits' i)))
  (defn count-on-bits [n]
    (+ (aget counts (bit-and n 0x0FF))
       (aget counts (bit-shift-right (bit-and n 0x0FF00) 8))
       (aget counts (bit-shift-right (bit-and n 0x0FF0000) 16))
       (aget counts (bit-shift-right (bit-and n 0x0FF000000) 24))
       (aget counts (bit-shift-right (bit-and n 0x0FF00000000) 32))
       (aget counts (bit-shift-right (bit-and n 0x0FF0000000000) 40))
       (aget counts (bit-shift-right (bit-and n 0x0FF000000000000) 48))
       (aget counts (bit-shift-right (bit-and n 0x7F00000000000000) 56)))))

(defn count-bin-ones "Number of ones below 2^n"
  [n] (->> (for [r (range 1 (inc n))]
           (* r (nCr n r)))
         (reduce +)))

(defn euler-049 [] nil)

