(ns project-euler.core)

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

(defn naturals [] (drop 1 (range)))

(defn sqr [x] (* x x))

(defn divisors [n]
  (let [low-divs  (filter #(divides? n %) (range 1 (zsqrt n)))
        high-divs (map #(quot n %) (rest low-divs))]
    (concat low-divs high-divs)))

(defn fib
  ([] (fib 0 1))
  ([x y] (lazy-seq (cons y (fib y (+' x y))))))

(defn up-to
  ([z]   (up-to 0 z))
  ([a z] (up-to a z 1))
  ([a z i] (range a (inc z) i)))

