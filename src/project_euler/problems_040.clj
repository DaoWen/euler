(ns project-euler.problems-040
  (:use project-euler.core)
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
    (for [i [1 2]] (conj (vec (split-at i ds)) others))))

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

