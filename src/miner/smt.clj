;;; http://johnj.com/from-elegance-to-speed.html
;;;
;;; The author of the above blog post says that his `smt-8` was slow so he re-wrote it in
;;; Common Lisp and got nearly 300x improvement.  I wrote some pure Clojure variations
;;; showing much improved performance over the original.
;;;
;;; Criterium for benchmarking: https://github.com/hugoduncan/criterium/

(ns miner.smt
  (:require [criterium.core :as cc]))


;; original
(defn smt-8 [times]
  (->> times
       (partition 8 1)
       (map (juxt identity
                  (comp (partial apply -)
                        (juxt last first))))
       (filter (comp (partial > 1000) second))))


;; modest improvement using `keep`, about 20% faster
(defn smt-8a [times]
  (->> times
       (partition 8 1)
       (keep (fn [part]
               (let [diff (- (last part) (first part))]
                 (when (> 1000 diff)
                   [part diff]))))))


;; for better performance, the original data should already by a vector

(defn smt-8for [times]
  (let [v (vec times)
        width 8]
    (for [i (range (- (count v) (dec width)))
          :let [diff (- (v (+ i (dec width))) (v i))]
          :when (> 1000 diff)]
      [(subvec v i (+ i width)) diff])))


(set! *unchecked-math* :warn-on-boxed)

(defn smt-8forh [v]
  (let [width 8]
    (for [^long i (range (- (count v) (dec width)))
          :let [diff (- ^long (v (+ i (dec width))) ^long (v i))]
          :when (> 1000 diff)]
      (list (subvec v i (+ i width)) diff))))


;; subvec will hold onto original v
;; if you don't want that, you can copy the subvec or take a slice out of v


;; faster, requires long hints
;; range expression compensates for "reverse" conjing into list, which is slightly faster
;; than conjing onto a vector.

(defn smt-8x [v]
  (let [width 8]
    (into ()
          (keep (fn [i]
                  (let [d (- ^long (v (+ (dec width) ^long i)) ^long (v i))]
                    (when (> 1000 d)
                      (list (subvec v i (+ ^long i width)) d)))))
          (range (- (count v) (inc width)) -1 -1))))


;; use Java array for speed
(defn smt-8arr [^longs larr]
  (let [width 8]
    (for [^long i (range (- (alength larr) (dec width)))
          :let [diff (- (aget larr (+ i (dec width))) (aget larr i))]
          :when (> 1000 diff)]
      (list (for [j (range i (+ i width))] (aget larr j)) diff))))

(set! *unchecked-math* false)



(defn my-bench []
  (let [big (into [] (take (long 1e6)) (iterate #(+ % (rand-int 1000)) 0))
        bar (long-array big)]
    (assert (= (smt-8 big) (smt-8a big) (smt-8for big) (smt-8forh big)
               (smt-8x big) (smt-8arr bar)))
    (println "Result count:" (count (smt-8arr bar)) "groups out of" (count big) "times")
    (doseq [smtf [smt-8 smt-8a smt-8for smt-8forh smt-8x]]
      (println)
      (println (type smtf))
      (cc/quick-bench (count (smtf big))))
    (println)
    (println (type smt-8arr))
    (cc/quick-bench (count (smt-8arr bar)))))

;; Function      Execution time mean (ms)
;; smt-8            1572
;; smt-8a           1341
;; smt-8for           47                 
;; smt-8forh          31
;; smt-8x             25
;; smt-8arr            7


