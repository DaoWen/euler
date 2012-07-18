(ns project-euler.problems-030
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

(defn digit-triplets [ds dset]
  (let [others (sort (apply disj dset ds))]
    (for [i (range 1 (count ds))]
      (conj (->> ds (split-at i) (map seq2num) vec) others))))

(defn euler-032
  "Find the sum of all numbers that can be written as pandigital products with digits 1-9."
  [] (let [ds (range 1 10), dset (set ds)]
       (->> (mapcat #(combinations ds %) [4 5])
            (mapcat permutations)
            (mapcat #(digit-triplets % dset))
            (keep (fn [[x y z]]
                    (let [p (* x y)]
                      (if (= z (-> p num2seq sort)) p))))
            distinct
            (reduce +))))

