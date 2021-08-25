(ns miner.edeal)

;;; Your task is to write a function that deals out the cards more evenly. That is, the
;;; first element goes into the first sequence, the second element goes into the second
;;; sequence, etc. We're going to write two versions of this function. Version one
;;; `deal-max` takes the maximum size of the subsequence. That means the number of
;;; subsequences will depend on the size of the given sequence.  Version two `deal-out`
;;; takes the number of subsequences. It is variable in the size of the subsequence.

;; lazy and pretty fast
(defn deal-out [n coll]
  (for [cs (take n (iterate rest coll))]
    (take-nth n cs)))

;; my initial take
(defn deal-out1 [n coll]
  (for [i (range n)]
    (take-nth n (drop i coll))))


;; slightly faster
(defn deal-out4 [n coll]
  (transduce (take n)
             (fn ([res x] (conj res (take-nth n x)))
               ([res] res))
             []
             (iterate rest coll)))

(defn ceil [n d]
  (if (zero? (rem n d))
    (quot n d)
    (inc (quot n d))))

;; faster, but not as pretty!
(defn mceil [^long n ^long d]
  (long (Math/ceil (/ (double n) (double d)))))

(defn deal-max [mx coll]
  (deal-out (ceil (count coll) mx) coll))



;; at most 3 elements in each subsequence
(defn smoke-deal-max [deal-max]
  (assert (= (deal-max 3 [1 2 3 4 5 6 7 8])  [[1 4 7] [2 5 8] [3 6]]))
  true)


;; deal out 4 subsequences
(defn smoke-deal-out [deal-out]
  (assert (= (deal-out 4 [1 2 3 4 5 6 7 8]) [[1 5] [2 6] [3 7] [4 8]]))
  (assert (= (deal-out 20 (range 20)) (map list (range 20))))
  (assert (= (first (deal-out 10 (range 1000))) (take 100 (iterate #(+ 10 %) 0))))
  true)
