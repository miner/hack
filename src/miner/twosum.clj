(ns miner.twosum
  (:require [clojure.math.combinatorics :as mc]
            [clojure.data.int-map :as im]))

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
  (assert (= (sort (twosum [1 5 12 6 7 0 3] 5)) '(0 5)))
  (assert (= (sort (twosum [1 5 2 6 7 20 3] 5)) '(2 3)))
  (assert (nil? (twosum [1 5 2 6 7 20 3] 15)))
  (assert (= (sort (twosum (cons 1 (range 0 100 2)) 51)) '(1 50)))
  (assert (= (sort (twosum (concat (range 0 101 2) '(1 3 8)) 103)) '(3 100)))
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


;;; sorted-set.  start at half, check for complement, dec

;;; Works but slow!
(defn twosum2 [nums target]
  (let [vset (into (sorted-set) nums)
        half (quot (inc target) 2)]
    (loop [vs (rsubseq vset <= half)]
      (when-first [v vs] 
        (if-let [v2 (vset (- target v))]
          [v v2]
          (recur (rest vs)))))))

(defn twosum3 [nums target]
  (let [vset (into (sorted-set) nums)
        half (quot (inc target) 2)]
    (loop [vs (subseq vset <= half)]
      (when-first [v vs] 
        (if-let [v2 (vset (- target v))]
          [v v2]
          (recur (rest vs)))))))

;; im/range not worth it
(defn twosum4 [nums target]
  (let [vset (into (im/dense-int-set) nums)
        half (quot (inc target) 2)]
    (loop [vs (im/range vset (first vset) half)]
      (when-first [v vs] 
        (if-let [v2 (vset (- target v))]
          [v v2]
          (recur (rest vs)))))))

;; BUG: what if target is negative?  can you quit at "half"?

;; fastest
(defn twosum5 [nums target]
  (let [vset (into (im/dense-int-set) nums)
        half (quot (inc target) 2)]
    (loop [vs vset]
      (let [v (first vs)]
        (when (and v (< v half))
          (if-let [v2 (vset (- target v))]
            [v v2]
            (recur (rest vs))))))))

(defn twosum6 [nums target]
  (let [vset (into (im/dense-int-set) nums)
        vmin (first vset)
        vmax (first (rseq vset))
        half (quot (inc target) 2)]
    (loop [vs vset]
      (let [v (first vs)]
        (when (and v (< v half))
          (let [v2 (- target v)]
            (if (vset v2)
              [v v2]
              (if (> v2 vmax)
            (recur (rest vs))))))))



(defn ss3 [nums target]
  (count (into (sorted-set) nums))
  (twosum nums target))

#_
(require '[clojure.math.combinatorics :as mc])
