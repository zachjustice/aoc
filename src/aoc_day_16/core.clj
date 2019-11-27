(ns aoc-day-16.core
  (:gen-class))
(def part1-filename "part1.txt")
(def part2-filename "part2.txt")

(defn mapify
  "take a single sample from the input and convert to a usable map"
  [arr]
  (def sublist-size 3)
  (for [index (range (/ (count arr) sublist-size))
        :let [start (* index sublist-size)
              end (+ (* index sublist-size) sublist-size)]]
    (do {:initial-registers  (get arr start)
         :op                 (get-in arr [(+ start 1) 0])
         :args               (subvec (get arr (+ start 1)) 1 4)
         :expected-registers (get arr (+ start 2))})))

(defn parse-line
  [line]
  (->> line
       (re-seq #"\d+")
       (map read-string)
       (vec)))

(defn listify-string
  [string]
  (map parse-line (filter #(not (clojure.string/blank? %1)) (clojure.string/split string #"\n"))))

(defn parse
  "output is list of samples [{:input [r1 r2 r3 r4] :op 'opcode A B C' :ouput [r1 r2 r3 r4]}]"
  [string]
  (mapify (vec (listify-string string))))

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
  "(add immediate) stores into register C the result of adding register A and value B."
  [a b c regs]
  (apply opi [+ a b c regs]))

(defn multr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [* a b c regs]))

(defn multi
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [* a b c regs]))

(defn banr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [bit-and a b c regs]))

(defn bani
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [bit-and a b c regs]))

(defn borr
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opr [bit-or a b c regs]))

(defn bori
  "(add register) stores into register C the result of adding register A and register B."
  [a b c regs]
  (apply opi [bit-or a b c regs]))

(defn setr
  "(set register) copies the contents of register A into register C. (Input B is ignored.)"
  [a b c regs]
  (assoc regs c (get regs a)))

(defn seti
  "(set immediate) stores value A into register C. (Input B is ignored.)"
  [a b c regs]
  (assoc regs c a))

(defn gtir
  "(greater-than immediate/register) sets register C to 1 if value A is greater than register B.
  Otherwise, register C is set to 0. "
  [a b c regs]
  (apply set-op [> a (get regs b) c regs]))

(defn gtri
  "(greater-than register/immediate) sets register C to 1 if register A is greater than value B.
  Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [> (get regs a) b c regs]))

(defn gtrr
  "(greater-than register/register) sets register C to 1 if register A is greater than register B.
  Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [> (get regs a) (get regs b) c regs]))

(defn eqir
  "(equal-than immediate/register) sets register C to 1 if value A is equal than register B.
  Otherwise, register C is set to 0. "
  [a b c regs]
  (apply set-op [= a (get regs b) c regs]))

(defn eqri
  "(equal-than register/immediate) sets register C to 1 if register A is equal than value B.
  Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [= (get regs a) b c regs]))

(defn eqrr
  "(equal register/register) sets register C to 1 if register A is equal to register B.
  Otherwise, register C is set to 0."
  [a b c regs]
  (apply set-op [= (get regs a) (get regs b) c regs]))

(defn apply-op-code
  [sample op]
  ;; (println sample)
  (let [args (:args sample)
        initial-registers (:initial-registers sample)
        expected-registers (:expected-registers sample)]
    ;; (println op)
    (try
      (do
        (def actual-registers (apply op (conj args initial-registers)))
        ;; (println actual-registers)
        ;; (println expected-registers)
        (if (= actual-registers expected-registers) 1 0))
      (catch Exception e
        (do 0))
      )))

(defn apply-op-codes
  "apply each opcode to the sample and determine if the expected register state matches the actual
  register state"
  [sample ops]
  (reduce + (map #(apply-op-code sample %1) ops)))

(defn tally-counts
  [counts]
  (->> counts
       (map #(if (>= %1 3) 1 0))
       (reduce +)))

(defn part1
  [file ops]
  (->> (parse (slurp file))
       (map #(apply-op-codes %1 ops))
       (tally-counts)))

;;;;;;;;;;;;;;;;

(defn apply-op
  [op args regs]
  (apply op (conj args regs)))

(defn is-valid-op-code
  [sample op]
  (let [args (:args sample)
        initial-registers (:initial-registers sample)
        expected-registers (:expected-registers sample)]
    (= (apply-op op args initial-registers) expected-registers)))

(defn find-valid-ops
  [sample ops]
  (filter #(is-valid-op-code sample %1) ops))

;; find valid op for each sample
;; map opcode -> op function
(defn assoc-opcode
  "take sample and ops list, return hash-map relating opcode num to applicable func's {1 (addi, addr, etc)}"
  [sample ops]
  (let [found-ops (find-valid-ops sample ops)
        opcode-num (sample :op)]
    (hash-map :opcode-num opcode-num :ops found-ops)))

(defn deduce
  "uses observations to update command-map. return updated command-map"
  [observations command-map]
  (reduce (fn [command-map observation]
            (let [{opcode-num :opcode-num ops :ops} observation]
              (if (= 1 (count ops))
                (do
                  (println "deduced:" (first ops) opcode-num)
                  (assoc command-map opcode-num (first ops) (first ops) opcode-num))
                command-map)))
          command-map
          observations))

(defn clean-up
  "uses updated command-map to clean up observations. return cleaned-up observations"
  [observations command-map]
  (as-> observations input
        (map (fn [observation]
               (let [non-deduced-ops (filter (fn [op] (not (command-map op))) (observation :ops))]
                 (println "removed:" (filter #(command-map %1) (observation :ops)))
                 (assoc observation :ops non-deduced-ops)))
             input)
        (filter #(< 0 (count (%1 :ops))) input)))

(defn deduce-ops
  [observations command-map]
  (if (= (count command-map) 32)
    command-map
    (let [new-cm (deduce observations command-map)]
      (println "---")
      (deduce-ops (clean-up observations new-cm) new-cm))))

(defn create-command-map
  [part1-filename ops]
  (as-> (parse (slurp part1-filename)) input
        (map #(assoc-opcode %1 ops) input)
        (deduce-ops input {})))

;;;;;;;;;;;;;;;;

(defn construct-command
  [command-map input]
  (let [op (get input 0)
        args (subvec input 1 4)]
    (hash-map
      :op (command-map op)
      :args args)))

(defn run-program
  [commands]
  (println commands)
  (reduce (fn [registers, command]
            (let [{op :op args :args} command]
              (apply-op op args registers)))
          [0 0 0 0]
          commands))

(defn part2
  [part2-filename command-map]
  (->> part2-filename
       (slurp)
       (listify-string)                                     ;; produce the ([op ...args] [] [])
       (map #(construct-command command-map %1))            ;; produce program ({:op :args} {})
       (run-program)                                        ;; takes the commands and produces the final registers
       (first)))

;;;;;;;;;;;;;;;;

(def ops [addi addr multi multr bani banr borr bori seti setr gtir gtri gtrr eqir eqri eqrr])
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "part 1 answer: " (part1 part1-filename ops))
  (println "part 2 answer: " (part2 part2-filename (create-command-map part1-filename ops)))
  )
