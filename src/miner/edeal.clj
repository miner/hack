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




;;; about same with into, slower with sequence but lazy is worth it
(defn deal-out2 [n coll]
  (for [i (range n)]
    (into [] (comp (drop i) (take-nth n)) coll)))

(defn deal-out23 [n coll]
  (for [i (range n)]
    (sequence (comp (drop i) (take-nth n)) coll)))


(defn deal-out3 [n coll]
  (mapv (fn [i] (into [] (comp (drop i) (take-nth n)) coll)) (range n)))

;; slightly faster
(defn deal-out4 [n coll]
  (transduce (take n)
             (fn ([res x] (conj res (take-nth n x)))
               ([res] res))
             []
             (iterate rest coll)))


(defn deal-out5 [n coll]
  (transduce (comp (take n) (map #(take-nth n %)))
             conj
             []
             (iterate rest coll)))

(defn deal-out6 [n coll]
  (transduce (map #(into [] (take-nth n) %))
             conj
             []
             (take n (iterate rest coll))))

(defn dout2 [n coll]
  (sequence (map #(sequence (comp (drop %) (take-nth n)) coll)) (range n)))

(defn roundup [n d]
  (if (zero? (rem n d))
    (quot n d)
    (inc (quot n d))))

;; faster, but not as pretty!
(defn rup [^long n ^long d]
  (long (Math/ceil (/ (double n) (double d)))))


(defn deal-max [mx coll]
  (deal-out (roundup (count coll) mx) coll))





;;; Not practical!  Idea is to make a `list` but exclude trailing nils.  BAD IDEA.  Assumes
;;; always one non-nil arg.  If any nil seen, the rest must be nil too!  Could use another
;;; marker to be safer, but it's already a bad idea so no need to be safe about it.
(defn lsome
  ([] ())
  ([a] (list a))
  ([a b] (if (nil? b) (list a) (list a b)))
  ([a b c] (cond (nil? b) (list a)
                 (nil? c) (list a b)
                 :else (list a b c)))
  ([a b c & more] (cond (nil? b) (list a)
                        (nil? c) (list a b)
                        ;; tricky -- must guarantee one non-nil on recursion
                        :else (list* a b (apply lsome c more)))))

;;; but no good padding with nil.  Way too much work to elide during construction.
(defn pdeal-out [mx coll]
  (apply map lsome (partition mx mx (repeat (dec mx) nil) coll)))
  




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


;; slower and uglier
(defn dout [^long n coll]
  (loop [res (vec (repeat n []))  i 0   cs coll]
    (if (empty? cs)
      res
      (recur  (update res i conj (first cs)) (rem (inc i) n) (rest cs)))))
