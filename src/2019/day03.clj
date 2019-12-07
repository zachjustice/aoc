(ns day03.core
  (:gen-class))

(require '[clojure.set :as set])

(def filename "day03.txt")

(defn parse-line [line]
  (map (fn [s]
         (vec
           (list
             (.substring s 0 1)
             (read-string (.substring s 1)))))
       (clojure.string/split line #",")))

(defn parse [file]
  (map parse-line (clojure.string/split-lines file)))

(defn next-pos [init v]
  (let [[x y] init
        [dir magnitude] v]
    (case dir
      "R" (for [x1 (take magnitude (range (+ x 1) (+ (+ x 1) magnitude)))]
            (do [x1 y]))
      "L" (for [x1 (take magnitude (range (- x 1) (- (- x 1) magnitude) -1))]
            (do [x1 y]))
      "D" (for [y1 (take magnitude (range (- y 1) (- (- y 1) magnitude) -1))]
            (do [x y1]))
      "U" (for [y1 (take magnitude (range (+ y 1) (+ (+ y 1) magnitude)))]
            (do [x y1])))))

(defn input-to-pos [input]
  (reduce (fn [acc elem]
            (concat acc
              (next-pos
                    (last acc)
                    elem)))
          (list [0, 0])
          input))

(defn common-elements [& colls]
  (let [freqs (map frequencies colls)]
    (mapcat (fn [e] (repeat (apply min (map #(% e) freqs)) e))
            (apply set/intersection (map (comp set keys) freqs)))))

(defn manh-dist [u v]
  (reduce +
          (map (fn [[a b]] (Math/abs (- a b)))
               (map vector u v))))

(defn list-to-path [l]
  (loop [[head & rest] (reverse l)
         hmap {}]
    (if rest
      (recur rest
             (if (contains? hmap head)
               hmap
               (assoc hmap head (list (count rest)))))
      hmap)))

(defn part1 [filename]
  (as-> filename input
        (slurp input)
        (parse input)
        (map input-to-pos input)
        (common-elements (first input) (second input))
        (map #(manh-dist [0, 0] %) input)
        (sort input)
        (second input)))

(defn closest-intersection [distances]
  (apply min (map (fn [ds] (reduce + ds)) distances)))

(defn part2 [filename]
  (as-> filename input
        (slurp input)
        (parse input)
        (map input-to-pos input)
        (merge-with into (list-to-path (first input)) (list-to-path (second input)))
        (closest-intersection (filter #(> (count %) 1) (vals input)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
