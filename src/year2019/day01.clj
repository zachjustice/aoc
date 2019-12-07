(ns day01.core
  (:gen-class))

(def filename "day01.txt")

(defn parse
  [string]
  (map read-string (clojure.string/split string #"\n")))

(defn mass-to-fuel
  [mass]
  (as-> mass x
        (/ x 3)
        (- x 2)
        (int x)))

(defn fuel-to-fuel
  [fuel]
  (if (<= fuel 0)
    0
    (+ fuel (fuel-to-fuel (mass-to-fuel fuel)))))

(defn part1
  [filename]
  (->> filename
       (slurp)
       (parse)
       (map mass-to-fuel)
       (reduce + )))

(defn part2
  [filename]
  (->> filename
       (slurp)
       (parse)
       (map mass-to-fuel)
       (map fuel-to-fuel)
       (reduce + )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
