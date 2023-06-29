(ns miner.twosum
  (:require [clojure.math.combinatorics :as mc]))

;;; Inspired by
;;; https://news.ycombinator.com/item?id=36498477

;;; This is a variation on the original problem because I didn't really like their
;;; goal of finding indices.  I modified the problem to be better for Clojure.

;;; Given a collection of integers and a target sum, find two integers that sum to the target.

;;; Do we allow negatives?  Original problem said all non-neg.  (Perhaps easier for C.)  The
;;; blog article allows negs.  Not sure it matters for Clojure.


(defn brute-twosum [nums target]
  (first (filter (fn [[a b]] (= target (+ a b))) (mc/combinations nums 2))))

;; not faster
(defn brute-twosum2 [nums target]
  (first (filter (fn [pair] (= target (reduce + pair))) (mc/combinations nums 2))))



(defn test-twosum [twosum]
  (assert (= (sort (twosum [1 5 2 6 7 0 3] 5)) '(0 5)))
  (assert (= (sort (twosum [1 5 2 6 7 20 3] 5)) '(2 3)))
  (assert (nil? (twosum [1 5 2 6 7 20 3] 15)))
  (assert (= (sort (twosum (cons 1 (range 0 100 2)) 51)) '(1 50)))
  (assert (= (sort (twosum (concat (range 0 101 2) '(1 3 5)) 103)) '(3 100)))
  true)



(defn twosum [nums target]
  (let [vnum (vec (sort nums))
        half (quot (inc target) 2)]
    (loop [i 0 j (dec (count vnum))]
      (when (> j i)
        (let [vi (vnum i) vj (vnum j)]
          (when (< vi half) 
            (let [c (compare (+ vi vj) target)]
              (cond (zero? c) [vi vj]
                    (pos? c) (recur i (dec j))
                    :else (recur (inc i) j)))))))))


;;; perhaps should consider starting indices around half target.


;;; ordered-set.  start at half, check for complement, dec



#_
(require '[clojure.math.combinatorics :as mc])
