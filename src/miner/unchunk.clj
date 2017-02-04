(ns miner.unchunk)


;;; 01/30/17  14:13 by miner -- hacking
;; http://dev.clojure.org/jira/browse/CLJ-2056

;; proposed
(defn seek
  "Returns first item from coll for which (pred item) returns true.
   Returns nil if no such item is present, or the not-found value if supplied."
  {:added "1.9"
   :static true}
  ([pred coll] (seek pred coll nil))
  ([pred coll not-found]
    (reduce (fn [_ x]
              (if (pred x)
                (reduced x)
                not-found))
              not-found coll)))


;; naive approach can be inefficient due to chunking -- might call pred on first 32 elements
;; and ignore results

(defn naive-first-by [pred coll]
  (first (filter pred coll)))

(defn fby [pred coll]
  (loop [coll coll]
    (when-let [coll (seq coll)]
      (if (pred (first coll))
        (first coll)
        (recur (next coll))))))

;; Stuart Sierra on Stack Overflow
;; http://stackoverflow.com/questions/3407876/how-do-i-avoid-clojures-chunking-behavior-for-lazy-seqs-that-i-want-to-short-ci

(defn SS-unchunk [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
            (SS-unchunk (next s))))))
;; SEM -- I think rest might be slightly lazier than next above, but that needs verification
;; OK -- I convinced myself that rest is lazier.  Next has to check if the rest is empty and
;; return nil.

(defn unchunk [s]
  (when-let [s (seq s)]
    (lazy-seq
      (cons (first s)
            (unchunk (rest s))))))

;; Note: (seq [1 2 3]) is chunked even though the vector itself is not, so you want to call
;; seq first.  It's a compromise, not to look for more chunks if the first part isn't.  Not
;; sure if that's better than the full unchunk.

(defn seq1 [s]
  (when-let [s (seq s)]
    (if (chunked-seq? s)
      (unchunk s)
      s)))

(defn ufirst-by [pred coll]
  (first (filter pred (seq1 coll))))

;;need to test 35 count to see if later chunkiness traps (32 is the usual chunksize)

;; suggested by @puredanger, about the same perf
(defn first-trans [pred coll]
  (transduce (comp (filter pred) (halt-when any?)) identity nil coll))


(defn firsome [pred coll]
    (when (seq coll)
      (if (pred (first coll))
        (first coll)
        (recur pred (next coll)))))

(comment

  (defn hack35 [x] (println "hack" x) (= x 35))
  (first (filter hack35 (range 100)))
  ;; will print hack 0 to 63 because of chunking

  (defn slow35 [x] (Thread/sleep 1) (= x 35))

  )
