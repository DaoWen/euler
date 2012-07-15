(ns project-euler.problems-030
  (:use project-euler.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Problem 021

(defn euler-021
  "Evaluate the sum of all the amicable numbers under 10000."
  ([] (euler-021 10000))
  ([n] (let [ds (->> (range 1 n)
                     (map #(filter (partial divides? %) (range 1 %)))
                     (map #(reduce + %))
                     (map vector (range 1 n))
                     (into #{}))
             dm (into {} ds)
             pairs (filter (fn [[a b]] (and (not= a b) (ds [b a]))) ds)]
        (reduce + (map first pairs)))))

