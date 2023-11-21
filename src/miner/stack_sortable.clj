(ns miner.stack-sortable
  (:require [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;;; https://en.wikipedia.org/wiki/Stack-sortable_permutation

;;; Note: this works for many collections, but not if coll contains the pattern 231
(defn bogus-stack-sort [coll]
  (loop [stack [] xs (seq coll) res []]
    (if (empty? xs)
      (into res (rseq stack))
      (let [p (peek stack)]
        (if (and p (> (first xs) p))
          (recur (pop stack) xs (conj res p))
          (recur (conj stack (first xs)) (rest xs) res))))))


(defn orig-bogus-stack-sort [coll]
  (loop [stack [] xs (seq coll) res []]
    (if (empty? xs)
      (into res (rseq stack))
      (let [p (peek stack)
            x (first xs)]
        (if (nil? p)
          (recur (conj stack x) (rest xs) res)
          (if (> x p)
            (recur (pop stack) xs (conj res p))
            (recur (conj stack x) (rest xs) res)))))))

;;; can't use reduce because sometimes you need to reconsider the x (when x>p)


(defn test-ss [ss]
  (remove #(= (ss %) (sort %)) (mc/permutations (range 1 4))))

;;; p is a permuation of 1..N
(defn contains231? [p]
  (some (fn [[a b c]] (< c a b)) (mc/combinations p 3)))

