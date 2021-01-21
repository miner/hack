(ns miner.enest)

;;; https://gist.github.com/ericnormand/965c5004944a62296c481aa9ac3fff5a

;;; Write a function that nests the elements of a list one level deeper by repeating that
;;; element inside a new list a given number of times.

;; TRIVIAL

(defn nest [xs n]
  (map #(repeat n %) xs))

(defn fnest [xs n]
  (for [x xs]
    (repeat n x)))

(defn smoke-nest
  ([] (smoke-nest nest))
  ([nest]
   (assert (= (nest [:a :b :c] 2) '((:a :a) (:b :b) (:c :c))))
   (assert (= (nest [] 10) ()))
   (assert (= (nest [1 2 3 4] 1) '((1) (2) (3) (4))))
   (assert (= (nest [1 2 3] 0) '(()()())))
   true))
