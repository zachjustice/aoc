(ns year2019.day06
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]))

(defn parse [d]
  (as-> d input
        (string/split input #"\n")
        (map #(string/split % #"\)") input)))

(def test-data
  (->> (io/resource "year2019/day06/test.txt")
       (slurp)
       (parse)))
(def data
  (->> (io/resource "year2019/day06/part1.txt")
       (slurp)
       (parse)))

(defn create-orbital-map [planet-pairing-list]
  (reduce (fn [acc curr]
            (let [[planet-a planet-b] curr
                  orbits (or (acc planet-a) [])
                  new-orbits (conj orbits planet-b)]
              (assoc acc planet-a new-orbits)))
          {}
          planet-pairing-list))

(defn get-depths [curr depths orbital-map]
  (if (nil? (orbital-map curr))
    depths
    (apply (partial assoc depths)
           (flatten (map
                      #(list % (inc (depths curr)))
                      (orbital-map curr))))))

(defn calc-depths [init-planet orbital-map]
  (loop [queue [init-planet]
         depths {init-planet 0}]
    (let [[curr & rest] queue
          depths depths]
      (if (nil? curr)
        depths
        (recur
          (apply (partial conj rest) (orbital-map curr))
          (get-depths curr depths orbital-map))))))

(defn to-queue-elements [planet path next-planets]
  (map #(hash-map :planet % :path (conj path planet)) next-planets))

(defn bfs [init-planet final-planet orbital-map]
  (loop [queue [(hash-map :planet init-planet :path [])]]
    (let [[curr & rest] queue
          {planet :planet path :path} curr
          next-planets (orbital-map planet)]
      (if (= planet final-planet)
        path
        (recur (apply (partial conj rest) (to-queue-elements planet path next-planets)))))))

;; hashmap {"PPP" '(AAA BBB)}
;; hashmap {"PPP" <Depth>}
;; bfs to construct depth
;; (calc-depths orbital-map {"COM" 0})
;; add orbiting planets to queue w/ depth = (inc curr-depth)
;; (reduce + (vals depths))
(defn part1 [data]
  (->> data
       (create-orbital-map)
       (calc-depths "COM")
       (vals)
       (reduce +)))

(defn common-ancestor [a b])
(defn part2 [d]
  (def orbital-path (create-orbital-map d))
  (def you-path (bfs "COM" "YOU" orbital-path))
  (def santa-path (bfs "COM" "SAN" orbital-path))
  (def common-ancestor-tuple (last (for [i (take (count you-path) (range))
                                         :let [u (get you-path i)
                                               s (get santa-path i)]
                                         :when (= u s)]
                                     [i u])))
  (def comm-anc-d (first common-ancestor-tuple))
  (def common-anc-val(second common-ancestor-tuple))
  (def you-moves (dec (- (count you-path) comm-anc-d)))
  (def san-moves (dec (- (count santa-path) comm-anc-d)))
  (+ you-moves san-moves))


(defn -main
  [& args]
  ;; (part1 data)
  ;; (part2 data)
  ;; (calc-depths "COM" (create-orbital-map test-data))
  (bfs "COM" "YOU" (create-orbital-map test-data))
  )
