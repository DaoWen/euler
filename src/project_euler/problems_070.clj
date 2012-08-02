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
  (loop [n 1, cubes (transient {})]
    (let [n3 (* n n n)
          k  (-> n3 str sort)
          v  (conj (cubes k) n3)]
      (if (= x (count v))
        (reduce min v)
        (recur (inc n) (assoc! cubes k v))))))

