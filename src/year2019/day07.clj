(ns year2019.day07
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)))

(defmethod print-method PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(def data
  (as-> (io/resource "year2019/day07/part1.txt") input
        (slurp input)
        (clojure.string/split input #",")
        (map read-string input)
        (vec input)
        ))

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

(defn get-param [instr-pntr program param-mode]
  (let [immediate (get program instr-pntr)]
    (if (= param-mode 1)
      immediate
      (get program immediate))))

(defn get-opcode [instr-val]
  (mod instr-val 100))

(defn handle-op
  [instr-pntr program param-modes]
  ;; return loop-recur state {instr-pntr=> next index program=>program state}
  (let [instr-val (get program instr-pntr)
        arg1 (get-param (+ instr-pntr 1) program (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) program (get param-modes 1))
        arg3 (get-param (+ instr-pntr 3) program 1)]
    (case (get-opcode instr-val)
      1 (do-op instr-pntr instr-val + arg1 arg2 arg3 program)
      2 (do-op instr-pntr instr-val * arg1 arg2 arg3 program)
      4 (assoc (create-state (+ instr-pntr 2) program) :output arg1))))

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
  (not (= 0 num)))

(defn is-zero [num]
  (= 0 num))

(defn handle-jump [op instr-pntr program param-modes]
  ;; handle jump-if-true and jump-if-false
  (let [arg1 (get-param (+ instr-pntr 1) program (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) program (get param-modes 1))]
    (if (op arg1)
      (create-state arg2 program) ;; if is-zero, instr pntr modified and we don't adv pntr
      (create-state (+ instr-pntr 3) program))))                ;; otherwise adv instr pntr

(defn handle-comp [op instr-pntr program param-modes]
  ;; handle less than & equals
  (let [instr-val (get program instr-pntr)
        arg1 (get-param (+ instr-pntr 1) program (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) program (get param-modes 1))
        destination-pntr (get program   (+ instr-pntr 3))]
    (if (op arg1 arg2)
      (create-state
        (determine-next-instr-pntr instr-pntr instr-val destination-pntr 1) ;; determine-next-instr-pntr increments instr pntr by 4
        (assoc program destination-pntr 1))
      (create-state
        (+ instr-pntr 4)
        (assoc program destination-pntr 0)))))                     ;; won't modify instr pntr since there are no ops for 0

(defn handle-instruction [opcode instr-pntr program param-modes input inputs]
  (case opcode
    (1 2 4) (handle-op instr-pntr program param-modes)
    3     (assoc
            (create-state
              (+ instr-pntr 2)
              (assoc program
                (get program (inc instr-pntr))
                input))
            :inputs (pop inputs))
    5     (handle-jump is-non-zero instr-pntr program param-modes)
    6     (handle-jump is-zero     instr-pntr program param-modes)
    7     (handle-comp <           instr-pntr program param-modes)
    8     (handle-comp =           instr-pntr program param-modes)))

(defn generate-phases [start end]
  (apply concat (apply concat (apply concat (apply concat (for [i (range start end)]
                                                            (for [j (range start end)
                                                                  :when (not (= i j))]
                                                              (for [k (range start end)
                                                                    :when (not (or (= k i) (= k j)))]
                                                                (for [l (range start end)
                                                                      :when (not (or (= l i) (= l j) (= l k)))]
                                                                  (for [m (range start end)
                                                                        :when (not (or (= m i) (= m j) (= m k) (= m l)))]
                                                                    (list i j k l m)))))))))))


(defn safe-pop [l]
  (if (empty? l)
    (cond
      (vector? l) []
      (list? l) '()
      (instance? PersistentQueue l) PersistentQueue/EMPTY)
    (pop l)))

(defn run-program
  [program original-inputs]
  (loop [state (hash-map :instr-pntr 0 :program program :inputs original-inputs :output nil)]
    (let [{instr-pntr :instr-pntr
           program :program
           inputs :inputs
           output :output} state
          instr-val (get program instr-pntr)
          opcode (get-opcode instr-val)
          param-modes (get-param-modes instr-val)
          input (first inputs)]                      ;; default input to 0 instead of nil incase we run into future 3's after the first initial 2
      (if (= opcode 99)
        output
        (as-> (handle-instruction opcode instr-pntr program param-modes input inputs) x
              (merge state x)
              (recur x))))))

(defn terminate-program? [instr-pntr program]
  (= (get program (+ instr-pntr 2))
     99))

(defn run-amps [program phases]
  (reduce (fn [last-output current-phase]
            (run-program program (list current-phase last-output)))
          0
          phases))

(defn have-all-programs-stopped? [program-states]
  (empty? (filter #(false? (:stop? %)) program-states)))

(defn run-program-2
  [starting-instr-pntr original-inputs program]
  ;; returns {:instr-pntr instr-pntr :program program :output output :stop? true}
  (loop [state (hash-map :instr-pntr starting-instr-pntr :program program :inputs original-inputs)]
    (let [{instr-pntr :instr-pntr
           program :program
           inputs :inputs} state
          instr-val (get program instr-pntr)
          opcode (get-opcode instr-val)
          param-modes (get-param-modes instr-val)
          input (first inputs)]                      ;; default input to 0 instead of nil incase we run into future 3's after the first initial 2
      (cond
        ;; terminate
        (= opcode 99) (hash-map :instr-pntr instr-pntr :program program :stop? true)
        ;; return program state including :output
        (= opcode 4) (assoc (handle-op instr-pntr program param-modes) :stop? false) ;; (terminate-program instr-pntr program)
        ;; waiting for input
        (and (= opcode 3) (empty? inputs)) {:instr-pntr instr-pntr :program program}
        ;; execute as normal
        :else (as-> (handle-instruction opcode instr-pntr program param-modes input inputs) x
                    (merge state x)
                    (recur x))))))

;; only run program if its not stopped, otherwise return the terminated program state with the previous output nulled
;; -- actually prob unnecessary to null output since 99 opcode will return nil output
(defn handle-run-program-2 [program-prev-state curr-inputs]
  (let [{program :program
         prev-instr-pntr :instr-pntr
         stop? :stop?
         last-output :last-output
         outputs :outputs} program-prev-state
        program-next-state (run-program-2 prev-instr-pntr curr-inputs program)
        output (:output program-next-state)
        new-outputs (if (nil? output) outputs (conj outputs output))]
    (if stop?
      (assoc program-prev-state :outputs nil)
      (assoc program-next-state :outputs new-outputs :last-output (or output last-output)))))

(defn construct-input [phases program-states amp-index]
  (let [prev-amp-index (mod (dec amp-index) (count program-states))
        prev-amp (get program-states prev-amp-index)
        prev-amp-outputs (:outputs prev-amp)
        prev-amp-output (peek prev-amp-outputs)]
    (if (empty? phases)
      (list prev-amp-output)
      (list (first phases) prev-amp-output))))

(defn update-program-states [program-states amp-index program-next-state]
  ;; updates program-states with the amp we just ran
  ;; as well as popping off the input from the prev amp's output that was used in the current amp's execution
  (let [num-states (count program-states)
        prev-amp-index (mod (dec amp-index) num-states)
        prev-amp (get program-states prev-amp-index)
        prev-amp-outputs (:outputs prev-amp)]
    (assoc (assoc-in program-states [prev-amp-index :outputs] (safe-pop prev-amp-outputs))
      amp-index
      program-next-state)))

;; loop over amps until each amp halts
(defn run-feedback-loop [init-program init-phases]
  ;; (assoc-in (vec (map #({:program init-program :instr-pntr 0 :stop? false}) init-phases)) [(dec (count init-phases)) :outputs] '(0))
  (loop [program-states [{:program init-program :instr-pntr 0 :stop? false} ;; A
                         {:program init-program :instr-pntr 0 :stop? false} ;; B
                         {:program init-program :instr-pntr 0 :stop? false} ;; C
                         {:program init-program :instr-pntr 0 :stop? false} ;; D
                         {:program init-program :instr-pntr 0 :stop? false :outputs '(0)}];; E, A needs to start with an input of 0 so pretend amp-E did that
         phases init-phases
         amp-index 0]
    (let [program-prev-state (get program-states amp-index)
          inputs (construct-input phases program-states amp-index)
          program-next-state (handle-run-program-2 program-prev-state inputs)
          last-amp-index (dec (count program-states))
          next-amp-index (mod (inc amp-index) (count init-phases))]
      (if (have-all-programs-stopped? program-states)
        (:last-output (get program-states last-amp-index))       ;; last output of Amp E
        (recur (update-program-states program-states amp-index program-next-state)
               (safe-pop phases)
               next-amp-index)))))

(defn part1 [program]
  (apply max (map (partial run-amps program) (generate-phases 0 5))))

(defn part2 [program]
  (apply max (map (partial run-feedback-loop program) (generate-phases 5 10))))

(defn -main
  [& args]
  ;; (part1 data)
  )
