(ns advent24.day2
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn]))


;;; https://adventofcode.com/2024/day/2
;;; using Google login

;;; file in hack/resources/
(def input-24-2-file-name "input24-2.txt")



;;; vectorize the input from the file
(defn load-input2 [filename]
  (with-open [in (io/reader (io/resource filename))]
    (mapv #(edn/read-string (str "[" % "]")) (line-seq in))))


;;; Safe levels require:
;;; The levels are either all increasing or all decreasing.
;;; Any two adjacent levels differ by at least one and at most three.


(defn safe-diffs? [levels]
  (boolean (reduce (fn [r x] (if (<= 1 (abs (- r x)) 3) x (reduced false))) levels)))

(defn safe-report? [levels]
  (or (< (count levels) 2)
      (and (safe-diffs? levels)
           (or (apply < levels) (apply > levels)))))


(def example
  [[7 6 4 2 1]
   [1 2 7 8 9]
   [9 7 6 2 1]
   [1 3 2 4 5]
   [8 6 4 4 1]
   [1 3 6 7 9]])

#_ (filter safe-report? example)


(defn day2-part1 []
  (count (filter safe-report? (load-input2 input-24-2-file-name))))

;; (day2-part1)
;; 224

;;; part 2.  Now, the same rules apply as before, except if removing a single level from an
;;; unsafe report would make it safe, the report instead counts as safe.

(defn excludev [levelv i]
  (reduce-kv (fn [r k x] (if (= k i) r (conj r x))) [] levelv))

;; slightly faster maybe
(defn excludev2 [levelv i]
  (into (subvec levelv 0 i) (subvec levelv (inc i))))

(defn safe-report2? [levelv]
  (some safe-report? (map #(excludev levelv %) (range (count levelv)))))

(defn day2-part2 []
  (count (filter safe-report2? (load-input2 input-24-2-file-name))))


;; (day2-part2)
;; 293

