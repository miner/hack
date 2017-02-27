;;; 02/21/17  17:00 by miner -- First of all, this is probably a bad idea.  So don't get too
;;; carried away with it.

;; By the way, clojure has a (sort), which is what you should use in real programs.  I'm pretty
;; sure mergesort is usually the best way to implement sorting in a functional style so
;; don't use this quicksort code.
;;
;; My best version is (iqsort coll) which I ported from the Hoare partition scheme explained
;; on Wikipedia [1].  I converted to an iterative approach to avoid the non-tail recursion.  It's
;; faster, but not as pretty as the original.  And, of course, the built-in Clojure `sort` is
;; still the best option.  This is purely an academic exercise.
;;
;; [1] https://en.wikipedia.org/wiki/Quicksort

(ns miner.quicksort)

;; Other examples of quicksort:
;; http://eddmann.com/posts/quicksort-in-clojure/
;; https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Clojure

;; SEM: but this is not the traditional quick-sort.  No swapping or taking into account the
;; physical location of the pivot.  Laziness might be nice if you only need the front end of
;; the sort.

(defn naive-quick-sort [[pivot & coll]]
  (when pivot
    (concat (naive-quick-sort (filter #(< % pivot) coll))
            [pivot]
            (naive-quick-sort (filter #(>= % pivot) coll)))))


;; for debugging with transient vector
(defn tseq [tv]
  (for [i (range (count tv))] (tv i)))


;; Trying to avoid recursion by keeping stack of bounds, taken in pairs of left and right
;; Inline assoc! seems slightly faster than calling out to tvexch!
;; Testing < left right before conj-ing new bounds is a bit faster that conj-ing and later
;; ignoring.

;; tv is transient vector
;; left and right are inclusive bounds, assumes (< left right)
;; stack holds inclusive bounds that still need to be processed.

;; There are a couple of variations below that specialize for numbers (comparing with > and
;; <).  Using a mutable Java array is faster than a Clojure vector.

;; Ported from Hoare partition given in Wikipedia and converted to iterative stack instead
;; of recursion.  Note, the partition is on J, not I as some other implementations give it.

;; Note: we use shadowing of locals to mimic the mutable implementations, but there
;; is no mutation going on.  The transient vector assoc! is destructive, but we always use
;; the result and don't depend on side-effect of possible mutation.

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

;; Specialized for longs. Significantly faster to use a mutable Java array of longs, but
;; less "functional style" (although the iqsort isn't very "functional" either).  Of course,
;; built-in (sort) is still much faster.
(defn arrqsort [coll]
  (let [arr (long-array coll)
        high (dec (alength arr))]
    (loop [stack (when (pos? high) (list 0 high))]
      (if (seq stack)
        (let [[left right & stack] stack
              pivot (aget arr (quot (+ left right) 2))
              stack (loop [i left j right]
                      (let [i (loop [i i] (if (< (aget arr i) pivot) (recur (inc i)) i))
                            j (loop [j j] (if (> (aget arr j) pivot) (recur (dec j)) j))]
                        (if (>= i j)
                          (cond-> stack
                            (< (inc j) right) (conj right (inc j))
                            (< left j) (conj j left))
                          (let [ai (aget arr i)]
                                           (aset arr i (aget arr j))
                                           (aset arr j ai)
                                           (recur (inc i) (dec j))))))]

          (recur stack))
        (seq arr)))))

;; What about general Clojure elements, not just numbers?  Need to sort with (compare).
(defn gqsort [coll]
  (let [v (vec coll)
        high (dec (count coll))]
    (loop [tv (transient v)
           stack (when (pos? high) (list 0 high))]
      (if (seq stack)
        (let [[left right & stack] stack
              pivot (tv (quot (+ left right) 2))
              [tv stack] (loop [i left j right tv tv]
                           (let [i (loop [i i] (if (neg? (compare (tv i) pivot)) (recur (inc i)) i))
                                 j (loop [j j] (if (pos? (compare (tv j) pivot)) (recur (dec j)) j))]
                             (if (>= i j)
                               [tv (cond-> stack
                                     (< (inc j) right) (conj right (inc j))
                                     (< left j) (conj j left))]
                               (recur (inc i) (dec j) (assoc! tv i (tv j) j (tv i))))))]
          (recur tv stack))
        (persistent! tv)))))



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



;; Don't bother with this one:
;; http://www.geeksforgeeks.org/iterative-quick-sort/
;; It looks good, but it's slow!  Maybe bad pivot choice? (at high)

(defn itpart [tv low high]
  (let [x (tv high)]
    (loop [i (dec low) j low tv tv]
      (if (< j high)
        (if (<= (tv j) x)
          (recur (inc i) (inc j) (assoc! tv (inc i) (tv j) j (tv (inc i))))
          (recur i (inc j) tv))
        ;; return
        [(inc i) (assoc! tv (inc i) (tv high) high (tv (inc i)))] ))))

(defn itqsort [coll]
  (let [v (vec coll)
        high (dec (count v))]
    (loop [stack (when (pos? high) (list 0 high)) tv (transient v)]
      (if (seq stack)
        (let [[low high & stack] stack
              [p tv] (itpart tv low high)
              stack (if (> (dec p) low) (conj stack (dec p) low) stack)
              stack (if (< (inc p) high) (conj stack high (inc p)) stack)]
          (recur stack tv))
        (persistent! tv)))))



(comment
  (qtest iqsort [4 3 2 1 4 3 2 1])

  (qtest iqsort (concat (range 13) (range 13) (range 13) (range 13)))
  
  (qtest iqsort (concat (range 20) (range 20) (range 20) (range 20)))

  (dotimes [n 1000] (rtest iqsort n))

  (require '[criterium.core :refer [quick-bench]])

  (quick-bench (qtest quicksort (concat (range 200 0 -1) (range 20) (repeat 200 42)
                                             (range 20 0 -1) (range 200))))


  (quick-bench (qtest quicksort (mapcat #(list % (inc %) %) (range 1000 0 -1))))

  )
