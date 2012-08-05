(ns project-euler.core
  (:import org.apache.commons.math.util.MathUtils))

(defn nCr [n r] (MathUtils/binomialCoefficient n r))

(defn zsqrt [n] (inc (int (Math/sqrt n))))

(defn divides? [x y] (zero? (mod x y)))

(defn chr2int [c] (- (int c) 48))

(def primes
  (cons 2
    ((fn p [i n]
       (lazy-seq
         (loop [n n]
           (if (not-any? #(divides? n %) (take-while #(< % (zsqrt n)) (take i primes)))
             (cons n (p (inc i) (+ 2 n)))
             (recur (+ 2 n)))))) 1 3)))

(defn prime? [n] (= n (first (drop-while #(> n %) primes))))

; Way faster and still very accurate
(defn prime? [n] (.isProbablePrime (biginteger n) 20))

(defn prime-factors [n]
  (loop [n n, [p & q :as ps] primes, acc []]
         (cond (= n 1)        acc
               (divides? n p) (recur (quot n p) ps (conj acc p))
               :else          (recur n q acc))))

(defn naturals [] (iterate inc 1))

(defn sqr [x] (* x x))

(defn divisors [n]
  (let [low-divs  (filter #(divides? n %) (range 1 (zsqrt n)))
        high-divs (map #(quot n %) (rest low-divs))]
    (concat low-divs high-divs)))

(defn fib
  ([] (fib 0 1))
  ([x y] (lazy-seq (cons y (fib y (+' x y))))))

(defn up-to "Range from 0 to n, inclusive."
  ([z]   (up-to 0 z))
  ([a z] (up-to a z 1))
  ([a z i] (range a (inc z) i)))

(defn seq2num [ds]
  (reduce (fn [acc d] (+ (* 10 acc) d)) 0 ds))

(defn num2seq [n]
  (loop [n n, acc ()]
    (if (zero? n) acc
      (recur (quot n 10) (conj acc (rem n 10))))))

; http://diditwith.net/2009/01/20/YAPESProblemSevenPart2.aspx
; http://stackoverflow.com/a/7625207/1427124
(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"
  [] (let [update!  (fn [m k f & vs] (assoc! m k (apply f (m k) vs)))
           reinsert (fn [m x p] (update! m (+ p x) conj p))]
       (defn primes-step [table d]
         (if-let [factors (get table d)]
           (recur (reduce #(reinsert %1 d %2) (dissoc! table d) factors) (inc d))
           (lazy-seq (cons d (primes-step (assoc! table (* d d) (list d)) (inc d))))))
       (primes-step (transient {}) 2)))

;; http://stackoverflow.com/a/1684988/1427124
;; Should be updated with future-cancel to actually kill the thread
(defmacro time-limited [ms & body]
  `(let [f# (future ~@body)]
     (.get f# ~ms java.util.concurrent.TimeUnit/MILLISECONDS)))

