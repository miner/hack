(ns miner.pythagorean
  (:require [clojure.math.combinatorics :as mc]))


;; "Scanning for Pythagorean triplets in Clojure"
;; https://clojure-diary.gitlab.io/2023/02/15/scanning-for-pythagorean-triplets-in-clojure.html
;; find_py_triplets.clj

;; Mostly bad example.  My solution is much simpler and faster.

;; SEM: note the correct spelling is "Pythagorean".  The orginal had it wrong.

(defn pythogorean-triplet?
  "Returns `true` if passed arguments are pythogorean triplets.
   
   **Usage**
   
   ```clojure
   (pythogorean-triplet? 3 4 5)
   ```"
  [a b c]
  (let [[x y z] (sort [a b c])]
    (=
     (* z z)
     (+
      (* x x)
      (* y y)))))

(defn number-combinations
  "
   Take in a range and returns combinations of numbers
   
   **Usage**
   
   ```clojure
   (number-combinations (range 1 3)) ; #{(1 1 1) (2 2 2) (1 1 2) (1 2 2)}
   ```"
  [nums]
  (set
   (for [x nums
         y nums
         z nums]
     (sort (list x y z)))))

(defn filter-triplets [num-combinations]
  "
   Given number combinations, filters out one those are
   pythogorean triplets
   
   **Usage**
   
   ```clojure
   filter-triplets [[1 2 3] [5 3 4]]) ; ([5 3 4])
   ```"
  (filter
   #(pythogorean-triplet?
     (first %)
     (nth % 1)
     (last %))
   num-combinations))

(defn find-py-triplets
  "
   Finds all Pythogorean Triplets berween `start` and `end`
   numbers.
   
   **Usage**
   
   ```clojure
   (find-py-triplets 1 10) ; ((3 4 5) (6 8 10))
   ```
  "
  [start end]
  (let [numbers (range start (inc end))
        combinations (number-combinations numbers)
        triplets (filter-triplets combinations)]
    (sort-by first triplets)))



;;; ----------------------------------------------------------------------

;;; much simpler and faster to use clojure.math.combinatorics
;;; note: mc/combinations generates in lexical order so you know the greatest is last
;;; note: original "end" is inclusive, which is not the normal Clojure convention
(defn sem-triplets [start end]
  (filter (fn [[a b c]] (= (* c c) (+ (* a a) (* b b))))
          (mc/combinations (range start (inc end)) 3)))


;;; If you don't want to use m.c.c/combinations, you can create the triplets by hand like this.
;;; Note: still tricky to have inclusive "end".  Triplet (size 3) is hard-wired into generation.
;;; See my combo.clj for inspiration.  Not recommended.

;; faster for larger inputs
(defn sem-triplets3 [start end]
  (loop [i 2 res (map vector (range start (dec end)))]
    (if (zero? i)
      (filter (fn [[a b c]] (= (* c c) (+ (* a a) (* b b)))) res)
      (recur (dec i)
             (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) (inc end)))) res)))))



#_
(quick-bench (find-py-triplets 1 10))
;; Execution time mean : 303.599879 µs

#_
(quick-bench (sem-triplets 1 10))
;; Execution time mean : 1.064362 µs

#_
(require '[clojure.math.combinatorics :as mc])
