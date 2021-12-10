(ns advent21.adv06)

;;; lanternfish

(defn lantern-dec [x]
  (if (zero? x)
    6
    (dec x)))

(defn lantern-step [lv]
  (into (mapv lantern-dec lv)
        (keep #(when (zero? %) 8) lv)))

(defn lantern [lv days]
  (count (first (drop days (iterate lantern-step lv)))))

#_
(lantern [3,4,3,1,2] 18)

(def lfish-input
  [3,3,2,1,4,1,1,2,3,1,1,2,1,2,1,1,1,1,1,1,4,1,1,5,2,1,1,2,1,1,1,3,5,1,5,5,1,1,1,1,3,1,1,3,2,1,1,
   1,1,1,1,4,1,1,1,1,1,1,1,4,1,3,3,1,1,3,1,3,1,2,1,3,1,1,4,1,2,4,4,5,1,1,1,1,1,1,4,1,5,1,1,5,1,1,
   3,3,1,3,2,5,2,4,1,4,1,2,4,5,1,1,5,1,1,1,4,1,1,5,2,1,1,5,1,1,1,5,1,1,1,1,1,3,1,5,3,2,1,1,2,2,1,
   2,1,1,5,1,1,4,5,1,4,3,1,1,1,1,1,1,5,1,1,1,5,2,1,1,1,5,1,1,1,4,4,2,1,1,1,1,1,1,1,3,1,1,4,4,1,4,
   1,1,5,3,1,1,1,5,2,2,4,2,1,1,3,1,5,5,1,1,1,4,1,5,1,1,1,4,3,3,3,1,3,1,5,1,4,2,1,1,5,1,1,1,5,5,1,
   1,2,1,1,1,3,1,1,1,2,3,1,2,2,3,1,3,1,1,4,1,1,2,1,1,1,1,3,5,1,1,2,1,1,1,4,1,1,1,1,1,2,4,1,1,5,3,
   1,1,1,2,2,2,1,5,1,3,5,3,1,1,4,1,1,4])


;;; uses too much memory for 256 days.  (That seems to be the lesson.)
;;; Had to change representation to N number of fish at each day remaining.
;;; That is, the vector index is how many days remaining and the value is the number of fish
;;; at that schedule.  The 0 index reproduces (adds to slot 8) and resets (adds num to
;;; position 6).  Note: you need a vector of size 9 (0-8 days).  Could use a subvec maybe.

(defn lf->vday [lv]
  (reduce (fn [dv d] (update dv d inc))
          (vec (repeat 9 0))
          lv))

(defn lant-vday-step [vd]
  (update (conj (into [] (rest vd)) (vd 0))
          6
          +
          (vd 0)))

(defn lant [lv days]
  (loop [d days vd (lf->vday lv)]
    ;;(println d vd)
    (if (zero? d)
      (reduce + 0 vd)
      (recur (dec d) (lant-vday-step vd)))))
