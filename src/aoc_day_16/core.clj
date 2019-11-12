(ns aoc-day-16.core
  (:gen-class))
(def filename "input.txt")

(defn mapify
  "take a single sample from the input and convert to a usable map"
  [arr]
  (def sublist-size 3)
  (vec (for [index (range (/ (count arr) sublist-size))
             :let [start (* index sublist-size)
                   end (+ (* index sublist-size) sublist-size)]]
         (do {:initial-registers (get arr start)
              :op     (get-in arr [(+ start 1) 0])
              :args   (subvec (get arr (+ start 1)) 1 4)
              :expected-registers  (get arr (+ start 2))}))))

(defn parse-line
  [line]
  (->> line
       (re-seq #"\d")
       (map read-string)
       (vec)))

(defn parse
  "output is list of samples [{:input [r1 r2 r3 r4] :op 'opcode A B C' :ouput [r1 r2 r3 r4]}]"
  [string]
  (def lines (vec (map parse-line (filter #(not (clojure.string/blank? %1)) (clojure.string/split string #"\n")))))
  (mapify lines))

(defn opr
  [op a b c regs]
  (let [register-a (get regs a)
        register-b (get regs b)]
    (assoc regs c (op register-a register-b))))

(defn opi
  [op a b c regs]
  (let [register-a (get regs a)]
    (assoc regs c (op register-a b))))

(defn set-op
  [op a b c regs]
  (assoc regs c (if (op a b) 1 0)))

(defn addr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [+ a b c regs]))

(defn addi
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [+ a b c regs]))

;; arr of each op
(defn multr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [* a b c regs]))

(defn multi
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [* a b c regs]))

;; arr of each op
(defn banr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [bit-and a b c regs]))

(defn bani
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [bit-and a b c regs]))

;; arr of each op
(defn borr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [bit-or a b c regs]))

(defn bori
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [bit-or a b c regs]))

;; arr of each op
(defn setr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (assoc regs c (get regs a)))

(defn seti
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (assoc regs c regs a))

(defn gtir
  "(greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0. "
  [a b c regs]
  (apply set-op [> a (get regs b) c regs]))

(defn gtri
  "(greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [> (get regs a) b c regs]))

(defn gtrr
  "(greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [> (get regs a) (get regs b) c regs]))

(defn eqir
  "(equal-than immediate/register) sets register C to 1 if value A is equal than register B. Otherwise, register C is set to 0. "
  [a b c regs]
  (apply set-op [= a (get regs b) c regs]))

(defn eqri
  "(equal-than register/immediate) sets register C to 1 if register A is equal than value B. Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [= (get regs a) b c regs]))

(defn eqrr
  "(equal-than register/register) sets register C to 1 if register A is equal than register B. Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [= (get regs a) (get regs b) c regs]))

(defn apply-op-code
  [sample op]
  (println "sample:" sample)
  (let [args (:args sample)
        initial-registers (:initial-registers sample)
        expected-registers (:expected-registers sample)]
    (def actual-registers (apply op (conj args initial-registers)))
    (if (= actual-registers expected-registers) 1 0)))

(defn apply-op-codes
  "apply each opcode to the sample and determine if the expected register state matches the actual register state"
  [sample ops]
  (reduce + (map #(apply-op-code sample %1) ops)))

(def ops [addi addr multi multr bani banr borr bori seti setr gtir gtri gtrr eqir eqri eqrr ])
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [samples (parse (slurp filename))]
    (map #(apply-op-codes %1 ops) samples)))

