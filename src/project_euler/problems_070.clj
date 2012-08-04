(ns project-euler.problems-070
  (:use [project-euler.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 061

(defn s-poly? [s x]
  "Check if x is an s-gonal polygonal number."
  (zero? (-> s (* 8) (- 16) (* x) (+ (sqr (- s 4)))
             Math/sqrt (+ s) (- 4) (rem (- (* 2 s) 4)))))

(defn s-poly [s n]
  "Get the nth s-gonal polygonal number."
  (- (* (dec (/ s 2)) (sqr n)) (* (- (/ s 2) 2 ) n)))

(defn four-digit-polys [s]
  (->> (iterate inc 1) (map #(s-poly s %))
       (drop-while #(< % 1e3)) (take-while #(< % 1e4))))

(defn split-num [n]
  (->> n num2seq (partition 2) (map seq2num)))

(defn- fpc [z x ps used?]
  (let [[_ b] (split-num x)]
    (if (seq ps)
      (for [p ps, y (four-digit-polys p)
            :when (not (used? y))
            :let [[a _] (split-num y)]
            :when (== a b)
            :let [cyc (fpc z y (remove #{p} ps) (conj used? y))]
            :when (seq cyc)] cyc)
      (let [[a _] (split-num z)]
        (if (== a b) [(conj used? z)])))))

(defn find-poly-cycle [ps]
  (let [[p & ps] ps
        xs (for [x (four-digit-polys p)
                 :let [cyc (fpc x x ps #{x})]
                 :when (seq cyc)] cyc)
        xs (-> xs flatten first)]
    [xs (reduce + xs)]))

(defn euler-061 []
  (find-poly-cycle (range 3 9)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 062

(defn euler-062 [x]
  (loop [n 1,
         cubes (transient {}),
         best Long/MAX_VALUE,
         lim Long/MAX_VALUE]
      (let [n3 (* n n n)
            k  (-> n3 str sort)
            v  (conj (cubes k) n3)
            m  (assoc! cubes k v)]
        (cond
          (> n3 lim) best
          (= x (count v)) (recur (inc n) m
                                 (min best (reduce min v))
                                 (Math/pow 10 (count k)))
          :else (recur (inc n) m best lim)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 063

(defn euler-063 []
  (->> (for [x (range 1 10)]
          (->> (iterate inc 1)
               (map #(-> [% (.pow (biginteger x) (biginteger %))]))
               (drop-while (fn [[n nx]] (< n (-> nx str count))))
               (take-while (fn [[n nx]] (= n (-> nx str count))))))
       (apply concat) count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 064
;; Reference:
;; www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/cfINTRO.html
;; (Especially section on square roots using algebra)

(defn sqrt-cont-frac-T
  "Period of the continued fraction for the square-root of n." [n]
  (let [sqrt-n (Math/sqrt n)]
    (if (zero? (rem sqrt-n 1)) 0
      (loop [i 1, x 1, y (long sqrt-n)]
        ; x / [ sqrt(n) - y ] ==> a + [ sqrt(n) - b ] / c
        (let [c (/ (- n (* y y)) x)
              a (long (/ (+ sqrt-n y) c))
              b (- (* a c) y)]
          (if (= 1 c) i (recur (inc i) c b)))))))

(defn euler-064
  "How many continued fractions for sqrt(N) where N<=10000 have an odd period?"
  ([] (euler-064 10000))
  ([n] (->> (range 2 (inc n)) (map sqrt-cont-frac-T) (filter odd?) count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 065

(defn cont-frac-e
  "Nth term of the continued fraction of e."
  [n] (let [ones   (repeat 1)
            two-ks (map #(* 2 %) (iterate inc 1))
            items  (cons 2 (interleave ones two-ks ones))]
        ((fn term [[x & xs] i]
           (if (= i n) x
             (+ x (/ 1 (term xs (inc i)))))) items 1)))

(defn euler-065
  "Find the sum of digits in the numerator of the 100th
   convergent of the continued fraction for e."
  ([] (euler-065 100))
  ([n] (->> n cont-frac-e numerator num2seq (reduce +))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 066

; Modified from p64's solution
(defn sqrt-terms
  "Convergents of the square-root of n" [n]
  (let [sqrt-n (Math/sqrt n)
        a0     (long sqrt-n)]
    (if (not= (* a0 a0) n)
      (loop [x 1, y a0, acc [a0]]
        ; x / [ sqrt(n) - y ] ==> a + [ sqrt(n) - b ] / c
        (let [c (/ (- n (* y y)) x)
              a (long (/ (+ sqrt-n y) c))
              b (- (* a c) y)
              acc (conj acc a)]
          (if (= 1 c) acc
            (recur c b acc)))))))

(sqrt-terms 7)

; Modified from p65's solution
(defn nth-sqrt-convergent
  "Nth convergent in the continued fraction of sqrt(x)." [x n]
  (let [[k & ks] (sqrt-terms x)
        items    (cons k (cycle ks))
        term     ((fn term [[x & xs] i]
                    (if (= i n) x
                      (+ x (/ 1 (term xs (inc i)))))) items 0)]
    (if (integer? term)
      [term 1]
      ((juxt numerator denominator) term))))

(nth-sqrt-convergent 7 0)

(defn search-xs [d]
  (loop [[x & xs] (iterate inc 2)
          sqrs    {1 1}]
    (let [xx (* x x)
          y  (-> xx dec (/ d))]
      (if (and (integer? y) (sqrs y)) x
        (recur xs (assoc sqrs xx x))))))

(defn euler-066
  ([] (euler-066 1000))
  ([n] (doall
         (for [d (range 2 (inc n))
               :let [x (search-xs d)]
               :when x]
           [d x]))))

#_(time (euler-066 14))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 067 (see problem 018)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 068

