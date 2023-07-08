(ns miner.twosum
  (:require [clojure.math.combinatorics :as mc]
            [clojure.data.int-map :as im]))

;;; Inspired by
;;; https://news.ycombinator.com/item?id=36498477

;;; This is a variation on the original problem because I didn't really like their
;;; goal of finding indices.  I modified the problem to be better for Clojure.

;;; Given a collection of integers and a target sum, find two integers that sum to the target.

;;; Do we allow negatives?  Original problem said all non-neg.  (Perhaps easier for C.)  The
;;; blog article allows negs?  Not sure.  Non-neg nums and target is easier.  Also assumes
;;; no duplicates.  V can't match with self.  But that means target 4 doesn't match [2 2]!


(defn brute-twosum [nums target]
  (first (filter (fn [[a b]] (= target (+ a b))) (mc/combinations nums 2))))

;; not faster
(defn brute-twosum2 [nums target]
  (first (filter (fn [pair] (= target (reduce + pair))) (mc/combinations nums 2))))



(defn test-twosum [twosum]
  (assert (= (sort (twosum [1 5 12 6 7 0 3] 5)) '(0 5)))
  (assert (= (sort (twosum [1 5 2 6 7 20 3] 5)) '(2 3)))
  (assert (nil? (twosum [1 5 2 6 7 20 3] 15)))
  (assert (nil? (twosum [1 2 5] 4)))
  (assert (= (sort (twosum [1 2 3 5] 4)) '(1 3)))
  (assert (= (sort (twosum [1 2 3 5] 5)) '(2 3)))
  (assert (nil? (twosum (range 0 100 2) 99)))
  (assert (= (sort (twosum (cons 1 (range 0 100 2)) 51)) '(1 50)))
  (assert (= (sort (twosum (concat (range 0 101 2) '(1 3 8)) 103)) '(3 100)))
  true)

;;; FIXME -- more Clojurish to take target first and nums second arg



;;; assume non-neg nums and target
;;; assume no duplicates in nums -- also means v can't match itself

;;; speed depends a lot of examples.  If nums is big and going to fail, it's worth getting
;;; them sorted (for "free" with int-set) so you only have to test half of them.  If things
;;; are smaller and going to match, sorting isn't worth the trouble as you'll likely get
;;; lucky during search.

;;; new fastest!
(defn twosum [nums target]
  (transduce identity
             (fn ([vset n]
                  (if-let [v (vset (- target n))]
                    (reduced (if (> v n) [n v] [v n]))
                    (conj vset n)))
               ([res] (when (vector? res) res)))
             (im/dense-int-set)
             nums))





(defn twosumx [nums target]
  (let [vset (into (im/dense-int-set) nums)
        half (quot (inc target) 2)]
    (transduce (take-while #(< % half))
               (fn ([r] r)
                 ([_ v] (when-let [v2 (vset (- target v))] (reduced [v v2]))))
               nil
               vset)))


(defn twosum-red [nums target]
  (let [vset (into (im/dense-int-set) nums)
        half (quot (inc target) 2)]
    (reduce (fn [_ v]
              (if (>= v half)
                (reduced nil)
                (when-let [v2 (vset (- target v))]
                  (reduced [v v2]))))
            nil
            vset)))

(defn ts1 [nums target]
  (let [res (reduce (fn [vset n]
                      (if-let [v (vset (- target n))]
                        (reduced [n v])
                        (conj vset n)))
                    #{}
                    nums)]
    (when (vector? res) res)))

;; fastest
(defn ts2 [nums target]
  (let [res (reduce (fn [vset n]
                      (if-let [v (vset (- target n))]
                        (reduced (if (> v n) [n v] [v n]))
                        (conj vset n)))
                    (im/dense-int-set)
                    nums)]
    (when (vector? res) res)))
                    

(defn ts3 [nums target]
  (let [res (reduce (fn [vset n]
                      (if-let [v (vset (- target n))]
                        (reduced (if (> v n) [n v] [v n]))
                        (conj vset n)))
                    (im/int-set)
                    nums)]
    (when (vector? res) res)))





(defn twosum1 [nums target]
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

;; was fastest
(defn twosum5 [nums target]
  (let [vset (into (im/dense-int-set) nums)
        half (quot (inc target) 2)]
    (loop [vs vset]
      (let [v (first vs)]
        (when (and v (< v half))
          (if-let [v2 (vset (- target v))]
            [v v2]
            (recur (rest vs))))))))


(defn twosum51 [nums target]
  (let [vset (into (im/dense-int-set) nums)
        half (quot (inc target) 2)]
    (loop [vs vset]
      (when-let [v (first vs)]
        (when (< v half)
          (if-let [v2 (vset (- target v))]
            [v v2]
            (recur (rest vs))))))))





;;; need to figure out bounds.  Also allow target to be negative and nums to be negative.
;;; Might have to got in reverse to be safe.

;;; if target is negative, start at the biggest and work to less than half

;;; start by checking extremes
;;; high + 2nd high < target --> nil
;;; low + 2nd low > target --> nil

;;; x + x2 > target ==> nil

;;; assuming non-neg TARGET

(defn twosum6 [nums target]
  (let [vset (into (im/dense-int-set) nums)]
    (loop [v (first vset) vs (rest vset)]
      (when v
        (let [v2 (- target v)]
          (if (vset v2)
            [v v2]
            (when-let [vn (second vs)]
              (when (> v2 vn)
                (recur (first vs) (rest vs))))))))))

;;; slow to disj -- better to depend on first/rest as in ts6
(defn twosum7-slow [nums target]
  (let [vset (into (im/dense-int-set) nums)]
    (loop [v (first vset) vs (disj vset (first vset))]
      (when v
        (let [v2 (- target v)]
          (if (vset v2)
            [v v2]
            (when-let [vn (second vs)]
              (when (> v2 vn)
                (recur (first vs) (disj vs (first vs)))))))))))




;;; for timing -- seems like sorted-set is slow.  dense-int-set is much faster
(defn ss3 [nums target]
  (count (into (sorted-set) nums))
  (twosum nums target))

#_
(require '[clojure.math.combinatorics :as mc])
