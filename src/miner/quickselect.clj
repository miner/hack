(ns miner.quickselect)

;;; find the kth smallest element in an unordered list (Clojure vector)
;;; Similar to QuickSort.  Also developed by Tony Hoare.
;;;
;;; https://en.wikipedia.org/wiki/Quickselect

;;; This code is not ready for prime time.  Probably better to sort and then select kth.

;;; Based on example code from:
;;; https://www.freecodecamp.org/news/quickselect-algorithm-explained-with-examples/

(defn naive-select [coll k]
  (first (drop k (sort coll))))

;;; much faster than any of the more clever attempts!
(defn fast-select [coll k]
  (nth (sort coll) k))

(defn quick-select-orig [coll k]
  (let [v (vec coll)
        pivotv (fn [v lo hi]
                 (let [vp (v lo)
                       i (loop [i lo] (if (< (v i) vp) (recur (inc i)) i))
                       j (loop [j hi] (if (> (v j) vp) (recur (dec j)) j))]
                   (if (>= i j)
                     [j v]
                     (recur (assoc v i (v j) j (v i)) i (dec j)))))
        select (fn [v k left right]
                 (if (= left right)
                   (v left)
                   (let [[pi v] (pivotv v left right)]
                     (if (= k pi)
                       (v k)
                       (if (< k pi)
                         (recur v k left (dec pi))
                         (recur v k (inc pi) right))))))]
    (select v k 0 (dec (count v)))))

;;; FIXED below, check if K < (count V)

(defn quick-select [coll k]
  (let [v (vec coll)
        cnt (count v)]
    (when (< k cnt)
      (loop [v v left 0 right (dec cnt)]
        (if (= left right)
          (v left)
          (let [[p v] (loop [v v lo left hi right]
                         (let [vp (v lo)
                               i (long (loop [i lo] (if (< (v i) vp) (recur (inc i)) i)))
                               j (long (loop [j hi] (if (> (v j) vp) (recur (dec j)) j)))]
                           (if (>= i j)
                             [j v]
                             (recur (assoc v i (v j) j (v i)) i (dec j)))))]
            (if (= k p)
              (v k)
              (if (< k p)
                (recur v (long left) (long (dec p)))
                (recur v (long (inc p)) (long right))))))))))

;;; transient v is a bit faster, less functional

(defn quick-select4 [coll k]
  (let [v (transient (vec coll))
        cnt (count v)]
    (when (< k cnt)
      (loop [left 0 right (dec cnt)]
        (if (= left right)
          (v left)
          (let [p (loop [lo left hi right]
                    (let [vp (v lo)
                          i (long (loop [i lo] (if (< (v i) vp) (recur (inc i)) i)))
                          j (long (loop [j hi] (if (> (v j) vp) (recur (dec j)) j)))]
                      (if (>= i j)
                        j
                        (do (assoc! v i (v j) j (v i))
                            (recur i (dec j))))))]
            (if (= k p)
              (v k)
              (if (< k p)
                (recur (long left) (long (dec p)))
                (recur (long (inc p)) (long right))))))))))




;;; Tried to pop/peek v-vector instead of nest.  Not better, roughly same speed. 


(def rrr (repeatedly 100 #(rand-int 100)))

(def qqq (repeatedly 5 #(rand-int 10)))

(def vvv [5 25 15 45 5])

(def zzz [2 1 1 3 7])
#_ (quick-select zzz 2)
#_ (map #(quick-select zzz %) (range 5))

(defn assert-qs [select vvv]
  (assert (= (map #(select vvv %) (range (count vvv))) (sort vvv)) vvv)
  true)

(defn test-qs [select]
  (assert-qs select vvv)
  (assert-qs select rrr)
  true)

