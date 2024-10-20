(ns miner.three-factor
  (:require [clojure.math.combinatorics :as mc]))

;;; https://news.ycombinator.com/item?id=41890158
;;; "List all the ways in which three distinct positive integers have a product of 108."
;;;
;;; Norvig reports on someone's attempt to get an AI chatbot to give the correct answer.  It
;;; worked better when Norvig asked the AI bots to write a program to solve the problem.

;;; I thought I should write such a program in Clojure

(defn three-factor [product]
    (for [a (range 1 (inc product))
          b (range (inc a) (inc product))
          c (range (inc b) (inc product)) :when (= product (* a b c))]
      [a b c]))


(def sol-108 [[1, 2, 54] [1, 3, 36] [1, 4, 27] [1, 6, 18] [1, 9, 12]
             [2, 3, 18] [2, 6, 9] [3, 4, 9]])



;;; better idea: do prime factorization and then find subsets.  Don't forget to allow 1.
;;; 108 = 2 × 2 × 3 × 3 × 3


(defn zmod? [x n]
  (zero? (rem x n)))

;;; works but slower
(defn factor3 [product]
  (for [a (range 1 (inc product))
        :when (zmod? product a)
        :let [qa (quot product a)]
        b (range (inc a) (inc product)) :when (zmod? qa b)
        c (range (inc b) (inc product)) :when (= product (* a b c))]
      [a b c]))



#_ (require '[clojure.math.combinatorics :as mc])

(defn factors [product]
  (filter #(zmod? product %) (range 1 (inc product))))

;;; nice but slower!
(defn factn [n product]
  (for [fs (mc/combinations (factors product) n)
        :when (= (apply * fs) product)]
    (vec fs)))

