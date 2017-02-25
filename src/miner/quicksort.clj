;;; 02/21/17  17:00 by miner -- First of all, this is probably a bad idea.  So don't get too
;;; carried away with it.

;; Tweet from @puffybsd that started me on this...
;; https://twitter.com/puffybsd/status/834089936223662082
;;
;; link to (bad) implementation of quicksort in Clojure
;; https://gist.github.com/spencer1248/2623396
;; which I copied into quicksort1.clj

;; These are my changes.  Still not sure quicksort makes sense for functional programming.

;; BTW, clojure has a (sort)
;; mergesort is usually the best way to implement sorting in a functional style
;; so this is just an academic exercise.


(ns miner.quicksort)

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



;; Better examples for quicksort
;; http://eddmann.com/posts/quicksort-in-clojure/
;; https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Clojure


;; SEM but this is not really quick-sort without the swapping and taking into account the
;; physical location of the pivot.  Seems more like a selection-sort.
;; All of these are slow and use stack because of explicit recursion.  Laziness might be
;; nice if you only need the front end of the sort.

(defn naive-quick-sort [[pivot & coll]]
  (when pivot
    (concat (naive-quick-sort (filter #(< % pivot) coll))
            [pivot]
            (naive-quick-sort (filter #(>= % pivot) coll)))))

(defn gb-quick-sort [[pivot & coll]]
  (when pivot
    (let [{lesser false greater true} (group-by #(> % pivot) coll)]
      (lazy-cat (gb-quick-sort lesser)
                [pivot]
                (gb-quick-sort greater)))))


;; My variation (not so fast, but bonus for when-first)
(defn fqs [coll]
  (when-first [pivot coll]
    (concat (fqs (filter #(< % pivot) (rest coll)))
            [pivot]
            (fqs (filter #(>= % pivot) (rest coll))))))


;; from Rosetta Code, lazy but very slow
(defn qsort3 [[pivot :as coll]]
  (when pivot
    (lazy-cat (qsort3 (filter #(< % pivot) coll))
              (filter #{pivot} coll)
              (qsort3 (filter #(> % pivot) coll)))))




;; C implementation from Rosetta Code
;;
;; void quicksort(int *A, int len)
;; {
;;   if (len < 2) return;
;;  
;;   int pivot = A[len / 2];
;;  
;;   int i, j;
;;   for (i = 0, j = len - 1; ; i++, j--)
;;   {
;;     while (A[i] < pivot) i++;
;;     while (A[j] > pivot) j--;
;;  
;;     if (i >= j) break;
;;  
;;     int temp = A[i];
;;     A[i]     = A[j];
;;     A[j]     = temp;
;;   }
;;  
;;   quicksort(A, i);
;;   quicksort(A + i, len - i);
;; }
 

;; faster to inline this
(defn tvexch! [tv i j]
  (assoc! tv i (tv j) j (tv i)))

;; for debugging with transient vector
(defn tseq [tv]
  (for [i (range (count tv))] (tv i)))



;; trying to avoid recursion by keeping stack of bounds, taken in pairs of left and right
;; Inline assoc! seems slightly faster than calling out to tvexch!
;; Testing < left right before conj-ing new bounds is a bit faster that conj-ing and later
;; ignoring.

;; tv is transient vector
;; left and right are inclusive bounds, assumes (< left right)
;; more is stack of bounds yet to be processed

(defn tvqs [tv [left right & more]]
  (let [[tv lrs] (let [pivot (tv (quot (+ left right) 2))]
                   (loop [i left j right tv tv]
                     (let [i (loop [i i] (if (< (tv i) pivot) (recur (inc i)) i))
                           j (loop [j j] (if (> (tv j) pivot) (recur (dec j)) j))]
                       ;; first test avoids infinite loop
                       (cond (= j left i) [tv more]
                             (>= i j) [tv (cond-> more
                                            (< i right) (conj right i)
                                            (< left (dec i)) (conj (dec i) left))]
                             :else (recur (inc i) (dec j)
                                          (assoc! tv i (tv j) j (tv i)))))))]

    (if (seq lrs)
      (recur tv lrs)
      tv)))


;; Reasonable fast, but not competitive with built-in sort
(defn myqsort [coll]
  (let [v (vec coll)
        cnt (count v)]
    (if (<= cnt 1)
      v
      (persistent! (tvqs (transient v) (list 0 (dec cnt)))))))



(defn qtest
  ([fsort] (qtest fsort (concat (range 100 0 -1) (range 100 0 -1))))
  ([fsort coll]
   (let [srt (fsort coll)]
     (or (= srt (sort coll))
         (throw (ex-info "Failed sort"
                         {:bad srt :coll coll :sort-function (str fsort)}))))))

(defn rtest
  ([fsort] (rtest fsort 1000))
  ([fsort cnt]
   (qtest fsort (repeatedly cnt #(rand-int cnt)))))



(comment
  (qtest myqsort [4 3 2 1 4 3 2 1])

  (qtest myqsort (concat (range 13) (range 13) (range 13) (range 13)))
  
  (qtest myqsort (concat (range 20) (range 20) (range 20) (range 20)))

  (require '[criterium.core :refer [quick-bench]])

  (quick-bench (qtest quicksort (concat (range 200 0 -1) (range 20) (repeat 200 42)
                                             (range 20 0 -1) (range 200))))


  (quick-bench (qtest quicksort (mapcat #(list % (inc %) %) (range 1000 0 -1))))

  (dotimes [n 1000] (rtest myqsort n))
  
  )


