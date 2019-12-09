(ns year2019.day09
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)))

(defmethod print-method PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(def data
  (as-> (io/resource "year2019/day09/part1.txt") input
        (slurp input)
        (clojure.string/split input #",")
        (map read-string input)
        (vec input)))

(defn parse [d]
  (as-> d input
        (clojure.string/split input #",")
        (map read-string input)
        (vec input)))

(defn parse
  [str]
  (map read-string (clojure.string/split str #",")))

(defn create-state [instr-pntr program]
  ;; state used by run-program-2's loop-recur
  (hash-map :instr-pntr instr-pntr :program program))

(defn create-state-with-inputs [instr-pntr program inputs]
  ;; state used by run-program-2's loop-recur
  (assoc (create-state instr-pntr program) :inputs (pop inputs)))

(defn determine-next-instr-pntr [instr-pntr instr-val destination-pntr instr-output]
  ;; don't adv instr pntr if instr pntr is modified.
  ;; determined by (instr-val != instr-output) && (destination-pntr == instr-pntr)
  (if (and (not (= instr-val instr-output)) (= destination-pntr instr-pntr))
    instr-pntr
    (+ instr-pntr 4)))

(defn do-op [instr-pntr instr-val op arg1 arg2 destination-pntr program]
  (let [output (op arg1 arg2)
        next-instr-pntr (determine-next-instr-pntr instr-pntr instr-val destination-pntr output)]
    (hash-map
      :instr-pntr next-instr-pntr
      :program (assoc program destination-pntr output))))

(defn get-param [arg-pntr relative-base program param-mode]
  (let [immediate (get program arg-pntr)]
    (case param-mode
      0 (get program immediate)                            ;; position mode
      1 immediate                                          ;; immediate mode
      2 (get program (+ immediate relative-base))          ;; relative mode
      )))

(defn get-param-for-input [arg-pntr relative-base program param-mode]
  (let [immediate (get program arg-pntr)]
    (case param-mode
      0 immediate                            ;; position mode
      ;; 1 immediate doesn't make sense for opcode 3 for input
      2 (+ immediate relative-base)          ;; relative mode
      )))

(defn get-opcode [instr-val]
  (mod instr-val 100))

(defn handle-op
  [instr-pntr relative-base program param-modes]
  ;; return loop-recur state {instr-pntr=> next index program=>program state}
  (let [instr-val (get program instr-pntr)
        arg1 (get-param (+ instr-pntr 1) relative-base program (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) relative-base program (get param-modes 1))
        arg3 (get-param-for-input (+ instr-pntr 3) relative-base program (get param-modes 2))]
    (case (get-opcode instr-val)
      1 (do-op instr-pntr instr-val + arg1 arg2 arg3 program)
      2 (do-op instr-pntr instr-val * arg1 arg2 arg3 program)
      4 (do (println arg1) (assoc (create-state (+ instr-pntr 2) program) :output arg1)))))

(defn get-param-modes [elem]
  (loop [n (int (/ elem 100))
         i 0
         acc [0 0 0]]
    (if (< n 10)
      (assoc acc i n)
      (let [digit (mod n 10)
            rem (int (/ n 10))]
        (recur rem (inc i) (assoc acc i digit))))))

(defn is-non-zero [num]
  (not (zero? num)))

(defn handle-jump [op instr-pntr relative-base program param-modes]
  ;; handle jump-if-true and jump-if-false
  (let [arg1 (get-param (+ instr-pntr 1) relative-base program (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) relative-base program (get param-modes 1))]
    (if (op arg1)
      (create-state arg2 program) ;; if is-zero, instr pntr modified and we don't adv pntr
      (create-state (+ instr-pntr 3) program))))                ;; otherwise adv instr pntr

(defn handle-comp [op instr-pntr relative-base program param-modes]
  ;; handle less than & equals
  (let [instr-val        (get program instr-pntr)
        arg1             (get-param (+ instr-pntr 1) relative-base program (get param-modes 0))
        arg2             (get-param (+ instr-pntr 2) relative-base program (get param-modes 1))
        destination-pntr (get-param-for-input (+ instr-pntr 3) relative-base program (get param-modes 2))]
    (if (op arg1 arg2)
      (create-state
        (determine-next-instr-pntr instr-pntr instr-val destination-pntr 1) ;; determine-next-instr-pntr increments instr pntr by 4
        (assoc program destination-pntr 1))
      (create-state
        (+ instr-pntr 4)
        (assoc program destination-pntr 0)))))                     ;; won't modify instr pntr since there are no ops for "0"

(defn handle-relative-base [instr-pntr relative-base program param-modes]
  (let [arg1 (get-param (+ instr-pntr 1) relative-base program (get param-modes 0))]
    (hash-map :relative-base (+ relative-base arg1) :instr-pntr (+ instr-pntr 2))))

(defn handle-instruction [opcode instr-pntr relative-base program param-modes input inputs]
  (case opcode
    (1 2 4) (handle-op instr-pntr relative-base program param-modes)
    3     (assoc
            (create-state
              (+ instr-pntr 2)
              (assoc program
                (get-param-for-input (inc instr-pntr) relative-base program (get param-modes 0))
                input))
            :inputs (pop inputs))
    5     (handle-jump is-non-zero instr-pntr relative-base program param-modes)
    6     (handle-jump zero?     instr-pntr relative-base program param-modes)
    7     (handle-comp <           instr-pntr relative-base program param-modes)
    8     (handle-comp =           instr-pntr relative-base program param-modes)
    9     (handle-relative-base    instr-pntr relative-base program param-modes)))

(defn pad-end [v count x]
  (apply (partial conj v) (repeat count x)))

(defn run-program
  ;; [program original-inputs] (run-program program original-inputs 0)
  [program original-inputs relative-base]
  (loop [state (hash-map :instr-pntr 0 :program (pad-end program (* 100 (count program)) 0) :inputs original-inputs :output nil :relative-base relative-base)]
    (let [{instr-pntr :instr-pntr
           program :program
           inputs :inputs
           output :output
           relative-base :relative-base
           } state
          instr-val (get program instr-pntr)
          opcode (get-opcode instr-val)
          param-modes (get-param-modes instr-val)
          input (first inputs)]                      ;; default input to 0 instead of nil incase we run into future 3's after the first initial 2
      (if (= opcode 99)
        output
        (as-> (handle-instruction opcode instr-pntr relative-base program param-modes input inputs) x
              (merge state x)
              (recur x))))))

(defn -main
  [& args]
  (run-program data '(1) 0))
