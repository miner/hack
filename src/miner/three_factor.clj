(ns miner.three-factor
  (:require [clojure.math.combinatorics :as mc]))

;;; https://news.ycombinator.com/item?id=41890158
;;; "List all the ways in which three distinct positive integers have a product of 108."
;;;
;;; Norvig reports on someone's attempt to get an AI chatbot to give the correct answer.  It
;;; worked better when Norvig asked the AI bots to write a program to solve the problem.

;;; I thought I should write such a program in Clojure

;;; slow
(defn three-factor [product]
    (for [a (range 1 (inc product))
          b (range (inc a) (inc product))
          c (range (inc b) (inc product)) :when (= product (* a b c))]
      [a b c]))


(def sol-108 [[1, 2, 54] [1, 3, 36] [1, 4, 27] [1, 6, 18] [1, 9, 12]
             [2, 3, 18] [2, 6, 9] [3, 4, 9]])



(defn zmod? [x n]
  (zero? (rem x n)))

;;; faster
(defn factor3 [product]
  (for [a (range 1 (inc product))
        :when (zmod? product a)
        :let [qa (quot product a)]
        b (range (inc a) (inc product)) :when (zmod? qa b)
        c (range (inc b) (inc product)) :when (= product (* a b c))]
      [a b c]))



#_ (require '[clojure.math.combinatorics :as mc])

;;; better idea from Norvig: do factorization and then find subsets.  Don't forget to allow 1.
;;; Note: I'm just finding integer factors, not prime factorization (with duplicates).  Not
;;; sure which way is better
;;; 108 = 2 × 2 × 3 × 3 × 3

;;; slightly faster
;;; all factors, not just prime

(defn factors [product]
  (conj (into [1] (filter #(zmod? product %)) (range 2 product)) product))

(defn factors2 [product]
  (filterv #(zmod? product %) (range 1 (inc product))))




;;; nice but slower!
(defn factn [n product]
  (for [fs (mc/combinations (factors product) n)
        :when (= (apply * fs) product)]
    fs))

;;; might be nicer to return vectors but doesn't really matter


;;; depends on factors being ordered which they are by construction
;;; fastest -- big win to prune with :while
(defn fact3 [product]
  (let [fs (factors product)]
    (for [a fs
          b fs :when (> b a) :let [ab (* a b)] :while (< ab product)
          c fs :when (> c b) :let [abc (* ab c)] :while (<= abc product)
          :when (= product abc)]
      [a b c])))


;;; was fastest -- faster than drop
(defn fact31 [product]
  (let [fs (factors product)]
    (for [a fs
          b fs :when (> b a)
          c fs :when (> c b) :when (= product (* a b c))]
      [a b c])))

;;; slightly faster but a little strange and not same order of results
;;; not enough faster to justify strangeness
(defn fact33 [product]
  (let [fv (factors product)]
    (for [a (rseq fv)
          b fv :while (< b a)
          c fv :when (> c b) :when (< c a) :when (= product (* a b c))]
      [b c a])))

;;; slightly slower
(defn fact32 [product]
  (let [fs (factors product)]
    (for [a fs
          b (drop-while #(<= % a) fs)
          c (drop-while #(<= % b) fs) :when (= product (* a b c))]
      [a b c])))



(defn sorted-factors [product]
  (into (sorted-set 1 product)
        (filter #(zmod? product %))
        (range 2 product)))

;; not faster
(defn fact3z [product]
  (let [fs (sorted-factors product)]
    (for [a fs
          b (subseq fs > a)
          c (subseq fs > b) :when (= product (* a b c))]
      [a b c])))


(defn fact3y [product]
  (let [fs (sorted-factors product)]
    (for [a fs
          b (subseq fs > a) :let [ab (* a b)] :while (< ab product)
          c (subseq fs > b) :let [p (* ab c)] :while (<= p product)
          :when (= p product)]
      [a b c])))
