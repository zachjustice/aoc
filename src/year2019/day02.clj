(ns day02.core
  (:gen-class))

(def input "1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,5,19,23,1,23,5,27,2,27,10,31,1,5,31,35,2,35,6,39,1,6,39,43,2,13,43,47,2,9,47,51,1,6,51,55,1,55,9,59,2,6,59,63,1,5,63,67,2,67,13,71,1,9,71,75,1,75,9,79,2,79,10,83,1,6,83,87,1,5,87,91,1,6,91,95,1,95,13,99,1,10,99,103,2,6,103,107,1,107,5,111,1,111,13,115,1,115,13,119,1,13,119,123,2,123,13,127,1,127,6,131,1,131,9,135,1,5,135,139,2,139,6,143,2,6,143,147,1,5,147,151,1,151,2,155,1,9,155,0,99,2,14,0,0")

(defn parse
  [str]
  (map read-string (clojure.string/split str #",")))

(defn do-op [op arg1 arg2 arg3 arr]
  (println op arg1 arg2 arg3 arr)
  (assoc arr arg3 (op (get arr arg1) (get arr arg2))))

(defn handle-op
  [curr-index elem arr]
  (let [arg1 (get arr (+ curr-index 1))
        arg2 (get arr (+ curr-index 2))
        arg3 (get arr (+ curr-index 3))]
    (case elem
      1 (do-op + arg1 arg2 arg3 arr)
      2 (do-op * arg1 arg2 arg3 arr)
      :else arr)))

(defn relevant-program [input]
  (take (.indexOf input 99) input))

(defn run-program
  [curr-index arr]
  (let [elem (get arr curr-index)]
    (case elem
      99 arr
      (1 2) (run-program (+ curr-index 4) (handle-op curr-index elem arr)))))

(defn part1
  [input]
  (as-> input input
        (parse input)
        (vec input)
        (run-program 0 input)
        (first input)))

(defn ans [noun verb] (+ verb (* 100 noun)))

(defn part2-helper [program noun verb needle fn]
  (if (> noun 99) false)
  (if (> verb 99) false)
  (if (= needle (try
                  (first (run-program 0 (assoc program 1 noun 2 verb)))
                  (catch NullPointerException e (println program noun verb))))
    (ans noun verb)
    (or (fn program (+ noun 1) verb needle fn)
        (fn program noun (+ verb 1) needle fn))))

(defn part2
  [input]
  (as-> input input
        (parse input)
        (vec input)
        (part2-helper input 0 0 19690720 (memoize part2-helper))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
