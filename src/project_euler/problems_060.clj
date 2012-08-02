(ns project-euler.problems-060
  (:use [project-euler.core]
        [project-euler.problems-030 :only (corners)]
        [clojure.set :only (map-invert)]
        [clojure.math.combinatorics :only (permutations combinations selections)]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 053

(defn euler-053
  "How many, not necessarily distinct, values of nCr,for 1<=n<=100, are greater than one-million?"
  ([] (euler-053 100 1e6))
  ([x l] (count (for [n (range 1 (inc x))
                      r (range 1 (inc n))
                      :let [c (try (nCr n r)
                                (catch ArithmeticException e (inc l)))]
                      :when (> c l)] c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 054

(def hand-type-score
  (->> [:high-card :1-pair :2-pairs :3-of-a-kind :straight :flush
        :full-house :4-of-a-kind :straight-flush :royal-flush]
       (map-indexed #(-> [%2 %])) (into {})))

(defn score-hand [hand]
  (let [flush?    (->> hand (map :suit) distinct count (= 1))
        ranks     (->> hand (map :rank) (sort >) vec)
        by-rank   (frequencies ranks)
        groups    (map-invert by-rank)
        [p1 p2]   (->> by-rank (keep (fn [[r f]] (if (= f 2) r))) (sort >))
        min-r     (ranks 4)
        max-r     (ranks 0)
        straight? (and (= 5 (count (distinct ranks))) (= 4 (- max-r min-r)))
        [r c]     (cond
                    flush? [(if straight?
                              (if (= 10 min-r) :royal-flush :straight-flush)
                              :flush) 0]
                    straight?  [:straight 0]
                    (groups 4) [:4-of-a-kind (groups 4)]
                    (groups 3) [(if p1 :full-house :3-of-a-kind) (groups 3)]
                    p2         [:2-pairs [p1 p2]]
                    p1         [:1-pair p1]
                    :else [:high-card 0])]
    {:hand-rank (hand-type-score r) :rank-cards c :all-cards ranks}))

(defn parse-hands [in]
  (let [rank (into {"T" 10 "J" 11 "Q" 12 "K" 13 "A" 14}
                   (map #(-> [(str %) %]) (range 2 10)))]
    (->> in (re-seq #"(\w)(\w)")
            (map (fn [[_ r s]] {:rank (rank r) :suit (symbol s)}))
            (partition 5) (partition-all 2))))

(defn euler-054
  "How many hands of poker does Player 1 win?"
  ([] (euler-054 (slurp "data/p054.txt")))
  ([in] (let [score-vec (juxt :hand-rank :rank-cards :all-cards)]
          (->> (parse-hands in)
               (map #(map (comp score-vec score-hand) %))
               (map #(apply compare %))
               (filter pos?)
               count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 055

(defn rev-num [n]
  (-> n num2seq reverse seq2num))

(defn palindrome? [n]
  (= n (rev-num n)))

(defn euler-055
  "How many Lychrel numbers are there below ten-thousand?"
  ([] (euler-055 10000))
  ([n] (count (for [i (range n)
                :let [xs (iterate #(+' % (rev-num %)) i)]
                :when (->> xs rest (take 50) (not-any? palindrome?))] i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 056

(defn euler-056 []
  (reduce max
    (for [i (range 1 100) :let [a (biginteger i)]
          j (range 1 100) :let [b (biginteger j)]]
      (->> (.pow a b) str (map chr2int) (reduce +)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 057

(defn euler-057 []
  (->> (iterate #(->> % inc (/ 1) inc) 3/2)
       (take 1000)
       (filter #(> (-> % numerator str count)
                   (-> % denominator str count)))
       count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 058
;; Uses "corners" function from problem 028

(defn euler-058 []
  (loop [diags (rest (corners)), ps 0, xs 1]
    (let [[heads tail] (split-at 4 diags)
          ps' (->> heads (filter prime?) count (+ ps))
          xs' (+ 4 xs)
          ratio (/ ps' (double xs'))]
      (if (< ratio 0.1) (-> xs' dec (/ 2) inc)
        (recur tail ps' xs')))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 059

(defn euler-059 []
  (let [chrs    (range (int \a) (inc (int \z)))
        data    (->> "data/p059.txt" slurp (re-seq #"\d+") (map #(Long/valueOf %)))
        words   ["an" "the" "ie" "ea" "er"]]
    (doall (for [password (selections chrs 3)
                 :let [res (map (comp char bit-xor) data (cycle password))
                       sum (reduce + (map int res))
                       ss  (.toLowerCase (apply str res))]
                 :when (not-any? #{\~ \| \< \> \^ \% \+ \=} res)
                 :when (not-any? #(neg? (.indexOf ss %)) words)]
             [(apply str (take 30 res)) sum password]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 060

(defn cat-primes
  "Groups of primes that can be concatenated to make more primes."
  ([n] (cat-primes n 1e3))
  ([n l] (cat-primes n l [] (cons 3 (drop 3 (gen-primes)))))
  ([n l group primes]
    (if (zero? n) group
      (let [catnums #(Long/valueOf (apply str %))]
        (loop [[p & ps] primes]
          (if (<= p l)
            (let [xs (mapcat #(map catnums [[% p] [p %]]) group)]
              (if-let [g (and (every? prime? xs)
                              (cat-primes (dec n) l (conj group p) ps))]
                g
                (recur ps)))))))))

(defn euler-060
  "Find a set of five primes for which any two primes
   concatenate to produce another prime."
  [] (cat-primes 5 1e4))

