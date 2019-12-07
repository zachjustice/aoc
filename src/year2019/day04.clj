(ns day04.core
  (:gen-class))

(def input "125730-579381")

(defn parse [input]
  (map read-string (clojure.string/split input #"-")))

(defn non-decreasing [num]
  (loop [n num]
    (if (< n 10) true
                 (let [x (mod n 10)
                       rem (int (/ n 10))]
                   (if (< x (mod rem 10))
                     false
                     (recur rem))))))

(defn add-to-acc [x head rest]
  (if (= x (first head))
    (cons (cons x head) rest)
    (cons x (cons head rest))))

(defn num-adj2 [num]
  (loop [n num
         acc '()]
    (if (< n 10) acc
                 (let [x (mod n 10)
                       rem (int (/ n 10))
                       [head & rest] acc]
                   (println x rem head rest)
                   (recur rem (add-to-acc x head rest))))))

(defn adj-counts [num]
  (map count (re-seq
               #"0{2,}|1{2,}|2{2,}|3{2,}|4{2,}|5{2,}|6{2,}|7{2,}|8{2,}|9{2,}"
               (str num))))

(defn num-adj [num limit]
  (let [counts (adj-counts num)]
    (< -1 (.indexOf counts limit))))

(defn two-adj [num]
  (loop [n num]
    (if (< n 10) false
                 (let [x (mod n 10)
                       rem (int (/ n 10))]
                   (if (= x (mod rem 10))
                     true
                     (recur rem))))))

(defn part1 [input]
  (let [[start end] (parse input)]
    (for [n (range start (+ end 1))
        :when (and (non-decreasing n) (two-adj n))]
     n)))

(defn part2 [input]
  (let [[start end] (parse input)]
    (for [n (range start (+ end 1))
          :when (and (non-decreasing n) (num-adj n 2))]
      n)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
