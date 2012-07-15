(ns project-euler.core)

(defn zsqrt [n] (inc (int (Math/sqrt n))))

(defn divides? [x y] (zero? (mod x y)))

(defn prime? [n] (and (> n 1) (not-any? #(divides? n %) (range 2 (zsqrt n)))))

(defn naturals [] (drop 1 (range)))

(defn sqr [x] (* x x))

