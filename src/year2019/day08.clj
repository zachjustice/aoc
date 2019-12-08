(ns year2019.day08
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)))

(defn parse [input]
  (as-> (map #(- (int %) (int \0)) input) input
        (vec input)))

(def w 25)
(def l 6)
(def data
  (as-> (io/resource "year2019/day08/part1.txt") input
        (slurp input)
        (parse input)))

(defn get-layers [data w l]
  (let [layer-length (* w l)]
    (for [i (range (/ (count data) layer-length))
          :let [start (* i layer-length)
                end   (+ start layer-length)]]
      (subvec data start end))))

(defn count-digits [digit coll]
  (count (filter #(= % digit) coll)))

(def count-zeros (partial count-digits 0))
(def count-ones (partial count-digits 1))
(def count-twos (partial count-digits 2))

(defn part1 [data w l]
  (let [layers (get-layers data w l)
        zero-counts (map count-zeros layers)
        min-zero-count (apply min zero-counts)
        min-zeros-index (.indexOf zero-counts min-zero-count)
        layers-with-min-zeros (nth layers min-zeros-index)
        one-count (count-ones layers-with-min-zeros)
        two-count (count-twos layers-with-min-zeros)]
    (* one-count two-count)))

;; construct vectors of pixels at each layer index
;; coallesce each pixel's vector
;; print
(defn get-pixel-vectors [layers]
  (for [index (range (count (first layers)))]
    (map #(get % index) layers)))

(defn coalesce [v]
  (first (filter #(not (= % 2)) v)))

(defn print-image [image row-length]
  (for [i (range (/ (count image) row-length))
        :let [start (* i row-length)
              end (+ start row-length)]]
    (println (apply str (map #(if (zero? %) " " "█" ) (subvec image start end))))))

(defn part2 [data w l]
  (let [layers (get-layers data w l)
        pixel-vectors (get-pixel-vectors layers)
        image (vec (map coalesce pixel-vectors))]
    (print-image image w)))

(defn -main
  [& args]
  (println (part1 data w l))
  (part2 data w l))
