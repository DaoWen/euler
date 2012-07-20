(ns project-euler.problems-040
  (:use project-euler.core)
  (:require [clojure.string :as string])
  (:use [clojure.set :only (intersection)])
  (:use [clojure.math.combinatorics :only (permutations combinations)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 031

(defn euler-031
  "How many different ways can 200p be made using any number of coins?
   (coins: 1p, 2p, 5p, 10p, 20p, 50p, 100p and 200p)"
  ([] (euler-031 [200 100 50 20 10 5 2] 200))
  ([[coin & coins] remainder]
     (if-not coin 1
       (->> (range 0 (inc remainder) coin)
            (map #(euler-031 coins (- remainder %)))
            (reduce +)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 032

(defn seq2num [ds]
  (reduce (fn [acc d] (+ (* 10 acc) d)) ds))

(defn num2seq [n]
  (loop [n n, acc ()]
    (if (zero? n) acc
      (recur (quot n 10) (conj acc (rem n 10))))))

(defn digit-triple [ds dset]
  (let [others (seq (apply disj dset ds))]
    (for [i [1 2]] [(take i ds) (drop i ds) others])))

(defn pandigital-product? [[x y z]]
  (let [p (* (seq2num x) (seq2num y))]
    (if (= z (-> p num2seq sort)) p)))

(defn euler-032
  "Find the sum of all numbers that can be written as pandigital products with digits 1-9."
  [] (let [ds (range 1 10), dset (into (sorted-set) ds)]
       (->> (combinations ds 5)
            (mapcat permutations)
            (mapcat #(digit-triple % dset))
            (keep pandigital-product?)
            distinct
            (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 033

(defn euler-033 []
  (let [xs (range 1 10)]
    (->> (for [a xs, b xs, c xs
               :let [ab    (+ (* 10 a) b)
                     bc    (+ (* 10 b) c)
                     ratio (/ ab bc)]
               :when (and (> 1 ratio) (= ratio (/ a c)))]
           ratio) (reduce *) denominator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 034

(defn euler-034
  "Find the sum of all numbers which are equal to the sum of the factorial of their digits."
  [] (let [f (into {} (map #(-> [% (reduce * (range 1 (inc %)))]) (range 10)))]
       (->> (range 10 (* 7 (f 9)))
            (keep #(if (= % (->> % num2seq (map f) (reduce +))) %))
            (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 035

(defn cycles [xs]
  (map-indexed (fn [n _] (concat (drop n xs) (take n xs))) xs))

(defn euler-035
  "How many circular primes are there below one million?"
  ([] (euler-035 1000000))
  ([n] (let [prime-set (set (take-while #(< % n) primes))
             circle #(->> % num2seq cycles (map seq2num))]
         (->> prime-set
              (filter #(every? prime-set (circle %)))
              count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 036

(defn euler-036
  "Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2."
  ([] (euler-036 1000000))
  ([n] (->> (range n)
            (map (juxt str #(Long/toString % 2)))
            (filter (partial every? #(= % (string/reverse %))))
            (map #(Long/valueOf (first %)))
            (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 037

(defn euler-037
  "Find the sum of the only eleven primes that are
   both truncatable from left to right and right to left."
  [] (let [imap   (fn [f xs] (map #(f % xs) (range 1 (count xs))))
           truncs #(let [xs (num2seq %)] (map seq2num (concat (imap take xs) (imap drop xs))))]
       (->> primes
            (drop-while #(< % 10))
            (filter #(every? prime? (truncs %)))
            (take 11)
            (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 038

(defn euler-038
  "What is the largest 1 to 9 pandigital 9-digit number that can be formed
   as the concatenated product of an integer with (1,2,..., n) where n>1?"
  [] (let [dset (set (range 1 10))]
       (->> (for [i (range 1 10000)]
              (loop [j 1, l 9, acc []]
                (cond (= 0 l) acc
                      (> 0 l) nil
                      :else (let [ds (num2seq (* i j))]
                              (recur (inc j)
                                     (- l (count ds))
                                     (apply conj acc ds))))))
            (filter #(= (set %) dset))
            (map seq2num)
            (reduce max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 039

(defn euler-039
  "If p is the perimeter of a right angle triangle with integral length
   sides, {a,b,c}, there are exactly three solutions for p = 120.
   For which value of p<=1000 is the number of solutions maximised?"
  ([] (euler-039 1000))
  ([n] (->> (for [a (range 1 500)
                  b (range a 500)
                  :let [c (Math/sqrt (+ (* a a) (* b b)))]
                  :when (== c (int c))
                  :let [p (+ a b (int c))]
                  :when (<= p 1000)] p)
            frequencies
            (reduce (partial max-key val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 040

(defn euler-040
  "An irrational decimal fraction is created by concatenating the positive integers.
   If d[n] represents the nth digit of the fractional part, find the value of the
   following expression: d[1]*d[10]*d[100]*d[1000]*d[10000]*d[100000]*d[1000000]"
  ([] (euler-040 #{1 10 100 1000 10000 100000 1000000}))
  ([is] (let [lim (reduce max is)] 
          (loop [[d & digits] nil, n 1, i 1, p 1]
            (cond (nil? d)  (recur (num2seq n) (inc n) i p)
                  (is i)    (recur digits n (inc i) (* p d))
                  (> i lim) p
                  :else     (recur digits n (inc i) p))))))

