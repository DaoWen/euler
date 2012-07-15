(ns project-euler.problems-010
  (:use project-euler.core))

;; Problem 001
;; Find the sum of all the multiples of 3 or 5 below 1000.

(let [m1k (fn [m] (reduce + (range 0 1000 m)))]
  (+ (m1k 3) (m1k 5) (- (m1k 15))))

;; Problem 002
;; By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

(letfn [(fib ([] (fib 1 1))
             ([x y] (lazy-seq (cons y (fib y (+ x y))))))]
  (->> (fib)
       (take-while #(< % 4000000))
       (filter even?)
       (reduce +)))

;; Problem 003
;; What is the largest prime factor of the number 600851475143 ?

(let [x 600851475143N
      factors (fn [n] (filter #(divides? n %) (range (zsqrt x) 0 -1)))]
  (->> x factors (filter prime?) first))

;; Problem 004
;; Find the largest palindrome made from the product of two 3-digit numbers.

(let [xs (range 999 0 -1)]
  (reduce max
    (for [x xs
          y xs
          :let [p (* x y)
                s (seq (str p))]
          :when (= s (reverse s))]
      p)))

;; Problem 005
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

(let [factors (range 2 21)
      has-factors? (fn [n fs] (every? #(divides? n %) fs))]
  (->> (iterate inc 20)
       (filter #(has-factors? % factors))
       (first)))

;; Problem 006
;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(let [sum-sqr #(reduce + (map sqr %))
      sqr-sum #(sqr (reduce + %))
      xs (range 101)]
  (- (sqr-sum xs) (sum-sqr xs)))

;; Problem 007
;; What is the 10,001st prime number?

(let [primes (filter prime? (naturals))]
  (->> primes (drop 10001) first))


;; Problem 008
;; Find the greatest product of five consecutive digits in the 1000-digit number.

(let [digits "73167176531330624919225119674426574742355349194934
              96983520312774506326239578318016984801869478851843
              85861560789112949495459501737958331952853208805511
              12540698747158523863050715693290963295227443043557
              66896648950445244523161731856403098711121722383113
              62229893423380308135336276614282806444486645238749
              30358907296290491560440772390713810515859307960866
              70172427121883998797908792274921901699720888093776
              65727333001053367881220235421809751254540594752243
              52584907711670556013604839586446706324415722155397
              53697817977846174064955149290862569321978468622482
              83972241375657056057490261407972968652414535100474
              82166370484403199890008895243450658541227588666881
              16427171479924442928230863465674813919123162824586
              17866458359124566529476545682848912883142607690042
              24219022671055626321111109370544217506941658960408
              07198403850962455444362981230987879927244284909188
              84580156166097919133875499200524063689912560717606
              05886116467109405077541002256983155200055935729725
              71636269561882670428252483600823257530420752963450"
      digits (map #(Integer/valueOf %) (re-seq #"\d" digits))]
  (->> digits
       (partition 5 1)
       (map #(reduce * %))
       (reduce max)))

;; Problem 009
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

(first
  (for [a (range 1 1000)
        b (range a (- 1000 a))
        :let [c (- 1000 a b)]
        :when (= (sqr c) (+ (sqr a) (sqr b)))]
    (* a b c)))


;; Problem 010
;; Find the sum of all the primes below two million.

(let [primes (filter prime? (range))]
  (reduce + (take-while #(< % 2000000) primes)))

