(ns project-euler.problems-400
  (:use project-euler.core)
  (:use [clojure.math.combinatorics :only (permutations combinations)])
  (:import project_euler.Crunch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 391

(defn count-on-bits [n]
  (loop [n (long n), k 0]
    (if (== 0 n) k
      (recur (bit-and n (dec n)) (inc k)))))

(defn S-k "Number of ones below 2^n"
  [n] (->> (for [r (range 1 (inc n))]
           (* r (nCr n r)))
         (reduce +)))

(defn S-seq []
  ((fn sk [k acc]
     (lazy-seq
       (let [digits (map chr2int (Long/toString k 2))
             ones   (reduce + digits)
             sum    (+ ones acc)]
         (cons [sum ones] (sk (inc k) sum))))) 0 0))

(defmacro pow2 [n]
  `(bit-shift-left 1 ~n))

(declare M)

(defn M 
  ([n] (M n n))
  ([n gap]
    (let [term (long (dec (pow2 (inc gap))))
          end  (long (- (S-k (inc gap)) gap 1))
          n (long n)]
      (loop [t (long (dec term)), x (long end), y (long 0), i (long n), mvs (long 0)]
        (cond
          (zero? t)   {:start y :end end :moves mvs} ; Success--found full move chain
          (< i (- n)) 0 ; Fail--no forcing move
          :else       (let [k (count-on-bits t)]
                        (if (neg? i) ; Found forcing move
                          (recur (dec t) (- x k) x (- n k) (inc mvs))
                          (recur (dec t) (- x k) y (- i k) mvs))))))))
(defn M'
  ([n] (M' n n))
  ([n gap]
    (let [term (dec (pow2 (inc gap)))
          end  (- (S-k (inc gap)) gap 1)]
      {:start (Crunch/crunch n (dec term) end 0 n)
       :end end})))

(def cube #(* % % %))

(defn p391
  ([x] (p391 x 25))
  ([x t]
    (->>
      (range 1 (inc x))
      (pmap (fn [n]
              (cond
                (< n t) (:start (M' n))
                (< n 2000) (:start (M' n t)))))
      doall
      (map cube)
      (reduce +))))

(defn euler-049 [] nil)

(comment

(map M' (range 1 25))

(map #(rem (- (S-k (inc %)) % 1) %) (range 1 25))

  (map count-on-bits (range 0 30))

  (->> (range 1 17) (map #(-> [% (:moves (M %))])))

  )

