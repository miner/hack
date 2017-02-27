;; Tweet from @puffybsd that started me on this...
;; https://twitter.com/puffybsd/status/834089936223662082
;;
;; link to (bad) implementation of quicksort in Clojure
;; https://gist.github.com/spencer1248/2623396
;;
;; At the top of this file is the original (bad) version.  After that, there's my cleaned up
;; version with some obvious improvements in style.  Then I got carried away and tried to write
;; a better Quicksort in Clojure.  For real programs, you should use the built-in Clojure
;; `sort` function, not anything you find here.

(use '[clojure.contrib.math :as math])

(defn my_quicksort [array]
  ; Create a new mutable data structure
  (def transientArray (transient array))

  (quicksort transientArray 0 (- (count array) 1))

  ; Return an immutable data structure
  (persistent! transientArray)
)

(defn quicksort [transientArray left right]
  (cond
    (< left right)
      (do
        (def pivotIndex (math/ceil (/ (+ left right) 2)))
        (def pivotNewIndex (qsPartition transientArray left right pivotIndex))
        (quicksort transientArray left (- pivotNewIndex 1))
        (quicksort transientArray (+ pivotNewIndex 1) right)
      ))
)

; Clojure already has a method called partition
(defn qsPartition [transientArray, left, right, pivotIndex]
  (def pivotValue (transientArray pivotIndex))

  ; Swap values in the transient vector
  (assoc! transientArray
    pivotIndex (transientArray right)
    right      (transientArray pivotIndex))

  (def storeIndex left)

  (doseq [i (range left right)]
    (cond
      (< (transientArray i) pivotValue)
        (do
          ; Swap values in the transient vector
          (assoc! transientArray
                  i          (transientArray storeIndex)
                  storeIndex (transientArray i))
          (def storeIndex (inc storeIndex)))))

  ; Swap values in the transient vector
   (assoc! transientArray
           storeIndex (transientArray right)
           right      (transientArray storeIndex))

  storeIndex
  )

;;;----------------------------------------------------------------------
;;; 02/21/17  17:02 by miner -- My fixes.  Still not good, but better style.


(defn ceil
  ([n] (long (Math/ceil n)))
  ([n d]
   (long (Math/ceil (/ (double n) (double d))))))

;; Swap values in the transient vector
(defn tswap! [transvec i j]
  (assoc! transvec i (transvec j) j (transvec i)))

(defn qsPartition [transvec, left, right, pivotIndex]
  (let [pivotValue (transvec pivotIndex)
        transvec  (tswap! transvec pivotIndex right)]
    (loop [transvec transvec i left storeIndex left]
      (cond (= i right) [(tswap! transvec storeIndex right)   storeIndex]

            (< (transvec i) pivotValue) (recur (tswap! transvec i storeIndex)
                                               (inc i) (inc storeIndex))

            :else  (recur transvec (inc i) storeIndex)))))


(defn qsort [transvec left right]
  (if (< left right)
    (let [pivotIndex (ceil (+ left right) 2)
          [transvec pivotNewIndex] (qsPartition transvec left right pivotIndex)]
      (-> transvec
          (qsort left (dec pivotNewIndex))
          (qsort (inc pivotNewIndex) right)))
    transvec))

(defn quicksort  [coll]
  (let [v (vec coll)
        transvec (transient v)]
    (persistent! (qsort transvec 0 (dec (count v))))))


;;; ----------------------------------------------------------------------
;;; 02/27/17  17:01 by miner -- here's a better implementation of an iterative quicksort for
;;; Clojure.  We use a stack to avoid non-tail recursion.  It's still much slower than the
;;; built-in (sort).

(defn iqsort [coll]
  (let [v (vec coll)
        high (dec (count coll))]
    (loop [tv (transient v)
           stack (when (pos? high) (list 0 high))]
      (if (seq stack)
        (let [[left right & stack] stack
              pivot (tv (quot (+ left right) 2))
              [tv stack] (loop [i left j right tv tv]
                           (let [i (loop [i i] (if (< (tv i) pivot) (recur (inc i)) i))
                                 j (loop [j j] (if (> (tv j) pivot) (recur (dec j)) j))]
                             (if (>= i j) [tv (cond-> stack
                                                (< (inc j) right) (conj right (inc j))
                                                (< left j) (conj j left))]
                                 (recur (inc i) (dec j)
                                        (assoc! tv i (tv j) j (tv i))))))]
          (recur tv stack))
        (persistent! tv)))))
