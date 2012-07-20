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
