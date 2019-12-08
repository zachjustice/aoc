(ns year2019.day05
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]))

(def data
  (as-> (io/resource "year2019/day05/part1.txt") input
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

(defn create-state [curr arr]
  ;; state used by run-program-2's loop-recur
  (hash-map :curr curr :arr arr))

(defn determine-next-instr-pntr [instr-pntr instr-val destination-pntr instr-output]
  ;; don't adv instr pntr if instr pntr is modified.
  ;; determined by (instr-val != instr-output) && (destination-pntr == instr-pntr)
  (if (and (not (= instr-val instr-output)) (= destination-pntr instr-pntr))
    instr-pntr
    (+ instr-pntr 4)))

(defn do-op [instr-pntr instr-val op arg1 arg2 destination-pntr arr]
  (let [output (op arg1 arg2)
        next-instr-pntr (determine-next-instr-pntr instr-pntr instr-val destination-pntr output)]
    (hash-map
      :curr next-instr-pntr
      :arr (assoc arr destination-pntr output))))

(defn get-param [instr-pntr arr param-mode]
  (let [immediate (get arr instr-pntr)]
    (if (= param-mode 1)
      immediate
      (get arr immediate))))

(defn get-opcode [instr-val]
  (mod instr-val 100))

(defn handle-op
  [instr-pntr arr param-modes]
  ;; return loop-recur state {curr=> next index arr=>program state}
  (let [instr-val (get arr instr-pntr)
        arg1 (get-param (+ instr-pntr 1) arr (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) arr (get param-modes 1))
        arg3 (get-param (+ instr-pntr 3) arr 1)]
    (case (get-opcode instr-val)
      1 (do-op instr-pntr instr-val + arg1 arg2 arg3 arr)
      2 (do-op instr-pntr instr-val * arg1 arg2 arg3 arr)
      4 (do (println arg1) (create-state (+ instr-pntr 2) arr)))))

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

(defn handle-jump [op instr-pntr arr param-modes]
  ;; handle jump-if-true and jump-if-false
  (let [arg1 (get-param (+ instr-pntr 1) arr (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) arr (get param-modes 1))]
    (if (op arg1)
      (create-state arg2 arr) ;; if is-zero, instr pntr modified and we don't adv pntr
      (create-state (+ instr-pntr 3) arr))))                ;; otherwise adv instr pntr

(defn handle-comp [op instr-pntr arr param-modes]
  ;; handle less than & equals
  (let [instr-val (get arr instr-pntr)
        arg1 (get-param (+ instr-pntr 1) arr (get param-modes 0))
        arg2 (get-param (+ instr-pntr 2) arr (get param-modes 1))
        destination-pntr (get arr   (+ instr-pntr 3))]
    (if (op arg1 arg2)
      (create-state
        (determine-next-instr-pntr instr-pntr instr-val destination-pntr 1) ;; determine-next-instr-pntr increments instr pntr by 4
        (assoc arr destination-pntr 1))
      (create-state
        (+ instr-pntr 4)
        (assoc arr destination-pntr 0)))))                     ;; won't modify instr pntr since there are no ops for 0

(defn part1
  [system-id arr]
  (def state {:curr 0 :arr arr})
  (loop [{instr-pntr :curr
         arr :arr} state]
    (let [elem (get arr instr-pntr)
          opcode (get-opcode elem)
          param-modes (get-param-modes elem)]
      (case opcode
        99 "done"
        (1 2 4) (recur (handle-op instr-pntr arr param-modes))
        3       (recur (create-state (+ instr-pntr 2) (assoc arr (get arr (inc instr-pntr)) system-id)))
        ))))

(defn part2
  [system-id arr]
  (loop [{instr-pntr :curr
          arr :arr} (hash-map :curr 0 :arr arr)]
    (let [elem (get arr instr-pntr)
          opcode (get-opcode elem)
          param-modes (get-param-modes elem)]
      (case opcode
        99 "done"
        (1 2 4) (recur (handle-op instr-pntr arr param-modes))
        3     (recur (create-state (+ instr-pntr 2) (assoc arr (get arr (inc instr-pntr)) system-id)))
        5     (recur (handle-jump is-non-zero instr-pntr arr param-modes))
        6     (recur (handle-jump is-zero     instr-pntr arr param-modes))
        7     (recur (handle-comp <           instr-pntr arr param-modes))
        8     (recur (handle-comp =           instr-pntr arr param-modes))
        ))))

(defn ans [noun verb] (+ verb (* 100 noun)))

(defn -main
  [& args]
  (part1 1 data)
  (println "---")
  (part2 5 data))
