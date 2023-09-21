(ns miner.prisoners)

;;; https://youtu.be/iSNsgj1OCLA?si=aXKFukYqDsDXZIPh

;;; Riddle of 100 prisoners by Veritasium.

;;; 100 prisoners, numbered 1 to 100
;;; one at a time, each enters room with 100 boxes
;;; He can open 50, looking for his own number
;;; He cannot change anything or communicate back to others.
;;; If all 100 succeed, they all go free.  Otherwise, they all are executed.
;;; Before the prisoners start, they may agree on a strategy.

;;; Surprising solution:  best policy is to open box with of your own number, then follow
;;; the number you find until you get yours or fail (50 boxes).  With this strategy,
;;; prisoners can all succeed about 30%.  Much better than random (50% per prisoner), which
;;; results in much less than 1% total success.

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


(defn all-loops [v]
  (loop [start 0 found #{} lps []]
    (if (= start (count v))
      lps
      (if (found start)
        (recur (inc start) found lps)
        (let [lp (find-loop v start)]
          (recur (inc start) (into found lp) (conj lps lp)))))))

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
