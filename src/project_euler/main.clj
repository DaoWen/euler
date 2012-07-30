(ns project-euler.main
  (:use project-euler.problems-400)
  (:gen-class))

(defn -main [& args]
  (let [[n x] args
        n   (if n (Long/valueOf n) 10)
        x   (if x (Long/valueOf x) 25)]
    (prn (time (p391 n x))))
  (shutdown-agents))

