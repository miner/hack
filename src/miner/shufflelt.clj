(ns miner.shufflelt)

;; https://lemire.me/blog/2017/09/18/visiting-all-values-in-an-array-exactly-once-in-random-order/

;;; Suppose that you want to visit all values in an array exactly once in “random order”.
;;; (Not actually random, but looks sort of "random".)  One way to do it is to use the fact
;;; that ((a x + b) modulo n) will visit all integer values in [0,n) exactly once as x
;;; iterates through the integers in [0, n), as long as a is coprime with n.


;;; Note: for standard Clojure, use (shuffle (range n)).  The rest of this is just an exercise.



(defn gcd ^long [^long a ^long b]
  (loop [a (abs a)
         b (abs b)]
    (if (zero? b)
      a
      (recur b (rem a b)))))

(defn coprime? [a b]
  (= (gcd a b) 1))

(defn select-coprime [n]
  (first (filter #(coprime? n %) (range (quot n 2) n))))


;;; rem is slightly faster than mod
;;; fastest
(defn once-lt [n]
  (let [a (select-coprime n)
        b (quot a 2)]
    (into [] (map #(rem % n)) (range b (+ b (* n a)) a))))


;;; a bit slower
(defn once-lt0 [n]
  ;(assert (> n 3))
  (let [a (select-coprime n)
        b (quot a 2)]
    (mapv (fn [x] (mod (+ (* a x) b) n)) (range n))))

;;; a bit slow
(defn once-lt1 [n]
  (let [a (select-coprime n)
        b (quot a 2)
        incr (fn ^long [^long x] (let [x1 (+ x a)] (if (< x1 n) x1 (- x1 n))))]
    (take n (iterate incr b))))

;;; pretty fast but not best anymore
(defn once-lt2 [n]
  (let [a (select-coprime n)]
    (loop [cnt (dec n) p (quot a 2) res []]
      (if (zero? cnt)
        (conj res p)
        (recur (dec cnt) (let [x (+ p a)] (if (< x n) x (- x n))) (conj res p))))))

;;; not so fast
(defn rand-lt [n]
  (seq (into #{} (range n))))

;;; idiomatic
(defn shuffle-lt [n]
  (shuffle (range n)))




