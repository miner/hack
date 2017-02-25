;; link to (bad) implementation of quicksort in Clojure
;; https://gist.github.com/spencer1248/2623396

;; It's ugly.  I don't care if it works.
;; DON'T CHANGE THIS ORIGINAL FILE


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

;;; 02/21/17  17:02 by miner -- My fixes will go in another file
