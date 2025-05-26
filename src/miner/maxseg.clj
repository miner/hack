(ns miner.maxseg)

;;; https://github.com/Engelberg/automata/blob/master/src/automata/core.clj


;; THE CLASSIC INTERVIEW PROBLEM - MAXIMUM SEGMENT SUM A popular problem is to find an O(n)
;; algorithm for computing the maximum sum achievable by adding up some contiguous
;; subsequence (aka segment) of a sequence of numbers (typical input is a mix of positive
;; and negative integers).

;; For example,
;; => (maximum-segment-sum [-1 2 3 -4 5 -8 4])
;; 6
;; because 2+3+-4+5 is 6

;;; The trick is to keep a running sum as you traverse the sequence, 
;;; never letting the running sum dip below 0.
(defn maximum-segment-sum [s] 
  (apply max (reductions (comp #(max 0 %) +) 0 s)))





(def example [-1 2 3 -4 5 -8 4])


;;; much slower, calculates a bunch of subvec sums
(defn maxss1 [s]
  (let [v (vec s)
        cnt (count v)]
    (apply max
           (for [i (range cnt)
                 j (range i cnt)]
             (apply + (subvec v i j))))))


;;; almost good
(defn maxss2 [s]
  (let [v (vec s)
        cnt (count v)]
    (reduce max 0
            (map (fn [end] (reduce (fn [sum x] (+ (max sum 0) x)) 0 (subvec v 0 end)))
                 (range 1 cnt)))))

;;; note:  (reduce max ...) is faster than (apply max ...)
;;; my single reduction keeps a stack of bests so far.  The current run is at the peek, but
;;; there maybe additional segment scores on the stack so you have to run max at the end.
;;; Slightly faster to check for non-negative x so you can pop bs and continue current run.

;;; fastest
(defn maxss [s]
  (reduce max
          (reduce (fn [bs x]
                    (let [b (peek bs)]
                      (if (neg? b)
                        (conj (pop bs) x)
                        (if (neg? x)
                          (conj bs (+ b x))
                          (conj (pop bs) (+ b x))))))
                  (list 0)
                  s)))

;;; tranducer version is also pretty good 
;;; actually faster on a big example
(defn maxsst [s]
  (transduce conj
             (fn ([bs x]
                  (let [b (peek bs)]
                    (if (neg? b)
                      (conj (pop bs) x)
                      (if (neg? x)
                        (conj bs (+ b x))
                        (conj (pop bs) (+ b x))))))
               ([bs] (reduce max bs)))
             (list 0)
             s))

