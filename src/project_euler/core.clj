(ns project-euler.core)

(defn zsqrt [n] (inc (int (Math/sqrt n))))

(defn divides? [x y] (zero? (mod x y)))

(defn prime? [n] (and (> n 1) (not-any? #(divides? n %) (range 2 (zsqrt n)))))

(defn naturals [] (drop 1 (range)))

(defn sqr [x] (* x x))

(defn divisors [n]
  (let [low-divs  (filter #(divides? n %) (range 1 (zsqrt n)))
        high-divs (map #(quot n %) (rest low-divs))]
    (concat low-divs high-divs)))

(defn fib
  ([] (fib 0 1))
  ([x y] (lazy-seq (cons y (fib y (+' x y))))))

