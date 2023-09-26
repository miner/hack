(ns miner.prisoners
 (:require [clojure.data.int-map :as im]))


;;; https://youtu.be/iSNsgj1OCLA?si=aXKFukYqDsDXZIPh

;;; Riddle of 100 prisoners by Veritasium.

;;; 100 prisoners, numbered 1 to 100
;;; one at a time, each enters room with 100 boxes
;;; He can open 50, looking for his own number
;;; He cannot change anything or communicate back to others.
;;; If all 100 succeed, they all go free.  Otherwise, they all are executed.
;;; Before the prisoners start, they may agree on a strategy.

;;; Searching at random should give about 50% per prisoner, which results in much less than
;;; 1% total success.  (0.5^100 = 7.88e-31)

;;; Surprising solution:  best policy is to open box with of your own number, then follow
;;; the number you find until you get yours or fail (50 boxes).  With this strategy,
;;; prisoners can all succeed about 30%.  

;;; See video for full explanation.  The trick comes down to considering the random sequence
;;; of 100 boxes as a series of loops.  If the longest loop is less then 50 boxes in length,
;;; the prisoners can win.  If it's longer than 50, they're already doomed.

;;; How do you know your number is going to be on the loop that starts with your box?  Each
;;; box is a pointer to the next in the loop.  Whichever one points to you must be in "your"
;;; loop.  In fact, it will be the last hop in the loop that "starts" at your index.  That's
;;; implied by the definition of a loop.


;;; For Clojure, it's natural to be zero-based so the prisoners and boxes are numbered 0-99

(defn rand100 []
  (vec (shuffle (range 100))))

(defn find-loop [v start]
  (loop [lp [start]]
    (let [x (v (peek lp))]
      (if (= x start)
        lp
        (recur (conj lp x))))))


(defn all-loops-good [v]
  (let [cnt (count v)]
    (loop [start 0 found #{} lps []]
      (if (= start cnt)
        lps
        (let [lp (find-loop v start)
              found (into found lp)]
          (recur (long (loop [i (inc start)] (if (found i) (recur (inc i)) i)))
                 found
                 (conj lps lp)))))))

;;; much faster with dense-int-set
(defn all-loops [v]
  (let [cnt (count v)]
    (loop [start 0 found (im/dense-int-set) lps []]
      (if (= start cnt)
        lps
        (let [lp (find-loop v start)
              found (into found lp)]
          (recur (long (loop [i (inc start)] (if (found i) (recur (inc i)) i)))
                 found
                 (conj lps lp)))))))

(defn all-loops000 [v]
  (loop [start 0 found #{} lps []]
    (if (= start (count v))
      lps
      (if (found start)
        (recur (inc start) found lps)
        (let [lp (find-loop v start)]
          (recur (inc start) (into found lp) (conj lps lp)))))))


(defn all-loops4 [v]
  (loop [start 0 found #{} lps []]
    (if (= start (count v))
      lps
      (if (found start)
        (recur (inc start) found lps)
        (let [lp (find-loop v start)]
          (recur (inc start) (into found lp) (conj lps lp)))))))



;;;; not better
(defn all-loops2 [v]
  (let [cnt (count v)
        next-start (fn [used? start]
                     (first (drop-while used? (range (inc start) (inc cnt)))))]
  (loop [start 0 found #{} lps []]
    (if (= start cnt)
      lps
      (let [lp (find-loop v start)
            found (into found lp)]
        (recur (long (next-start found start)) found (conj lps lp)))))))

(defn all-loops21 [v]
  (let [cnt (count v)
        next-start (fn [used? start]
                     (first (into nil (comp (drop-while used?) (take 1))
                                      (range (inc start) (inc cnt)))))]
  (loop [start 0 found #{} lps []]
    (if (= start cnt)
      lps
      (let [lp (find-loop v start)
            found (into found lp)]
        (recur (long (next-start found start)) found (conj lps lp)))))))

(defn all-loops22 [v]
  (let [cnt (count v)
        next-start (fn [used? start]
                     (loop [i start]
                       (if (used? i) (recur (inc i)) i)))]
  (loop [start 0 found #{} lps []]
    (if (= start cnt)
      lps
      (let [lp (find-loop v start)
            found (into found lp)]
        (recur (long (next-start found start)) found (conj lps lp)))))))

;;; fastest
(defn all-loops23 [v]
  (let [cnt (count v)]
    (loop [start 0 found #{} lps []]
      (if (= start cnt)
        lps
        (let [lp (find-loop v start)
              found2 (into found lp)]
          (recur (long (loop [i (inc start)] (if (found2 i) (recur (inc i)) i)))
                 found2
                 (conj lps lp)))))))

;;; not any faster than 23
(defn all-loops24 [v]
  (let [cnt (count v)]
    (loop [start 0 found (transient #{}) lps []]
      (if (= start cnt)
        lps
        (let [lp (find-loop v start)
              found2 (reduce conj! found lp)]
          (recur (long (loop [i (inc start)] (if (found2 i) (recur (inc i)) i)))
                 found2
                 (conj lps lp)))))))





;;; much slower
(defn all-loops3 [v]
  (loop [start 0 avail (set (range (count v))) lps []]
    (let [lp (find-loop v start)
          avail2 (reduce disj avail lp)
          start2 (first avail2)]
      (if start2
        (recur (long start2) (disj avail2 start2) (conj lps lp))
        (conj lps lp)))))

;;; pretty good but second best time
(defn all-loops33 [v]
  (loop [start 0 avail (im/dense-int-set (range (count v))) lps []]
    (let [lp (find-loop v start)
          avail2 (reduce disj avail lp)
          start2 (first avail2)]
      (if start2
        (recur (long start2) (disj avail2 start2) (conj lps lp))
        (conj lps lp)))))

;;; transients help but no way to select first without persistent!  still slower than orig
(defn all-loops31 [v]
  (loop [start 0 avail (reduce conj! (transient #{}) (range (count v))) lps []]
    (let [lp (find-loop v start)
          avail2 (persistent! (reduce disj! avail lp))
          start2 (first avail2)]
      (if start2
        (recur (long start2) (disj! (transient avail2) start2) (conj lps lp))
        (conj lps lp)))))





(defn safe100? [v]
  (<= (reduce max (map count (all-loops v))) 50))

(defn safe? [v]
  (let [half (quot (inc (count v)) 2)]
    (<= (reduce max (map count (all-loops v))) half)))

(defn prisoner-search [v p]
  (let [limit (quot (inc (count v)) 2)]
    (loop [b p cnt 1]
      (when (<= cnt limit)
        (let [x (v b)]
          (if (= x p)
            [b p cnt]
            (recur x (inc cnt))))))))
      


#_ (frequencies (map safe100? (repeatedly 10000 rand100)))
;;=>  {false 6887, true 3113}
