(ns miner.memtrack
  (:require [clj-memory-meter.trace :as cmm.trace]
            [clojure.math.combinatorics :as mc]))


;;; Original blog post
;;; https://clojure-goes-fast.com/blog/tracking-memory-usage/

(defn matches? [s]
  (= (mod (reduce * s) 7) 1))

;; Attach 10 megabytes of useless metadata to a subset to make it more
;; pronounced when it is kept in memory.
(defn add-deadweight [s]
  (with-meta s {:deadweight (byte-array (* 10 1024 1024))}))

(defn subsets-eager [[x & rst :as coll]]
  (if (empty? coll)
    [#{}]
    (let [rest-subsets (subsets-eager rst)]
      (into rest-subsets
            (map #(add-deadweight (conj % x)) rest-subsets)))))

#_ (count (filter matches? (subsets-eager (range 2 7))))
;;=> 6

#_ (count (filter matches? (subsets-eager (range 2 11))))
;;=> java.lang.OutOfMemoryError


(comment
  
  (require '[clj-memory-meter.trace :as cmm.trace])
  (require '[clojure.data.combinatorics :as combo])

(cmm.trace/trace-var #'matches?)
(cmm.trace/trace-var #'subsets-eager)

(cmm.trace/with-relative-usage
  (count (filter matches? (subsets-eager (range 2 7)))))

)

;;; [trace]
;;; (example/matches? <10.0 MiB>) | Heap: +372.1 MiB (+9.1%)
;;; │ └─→ <16 B> | Heap: +372.1 MiB (+9.1%)
;;; Final used heap: +75.6 KiB (+0.0%)

(defn subsets-lazy [[x & rst :as coll]]
  (if (empty? coll)
    [#{}]
    (let [rest-subsets (subsets-lazy rst)]
      (concat rest-subsets
              (map #(add-deadweight (conj % x)) rest-subsets)))))


(comment 
(cmm.trace/trace-var #'subsets-lazy)

(cmm.trace/with-relative-usage
  (count (filter matches? (subsets-lazy (range 2 7)))))
)


(defn subsets-lazy2 [coll]
  (let [v (vec coll)
        n (count v)]
    (map (fn [i]
           (->> v
                (keep-indexed (fn [idx item]
                                (when (bit-test i idx)
                                  item)))
                set
                add-deadweight))
         (range (bit-shift-left 1 n)))))


(comment
  
(cmm.trace/trace-var #'subsets-lazy2)

(cmm.trace/with-relative-usage
  (count (filter matches? (subsets-lazy2 (range 2 9)))))

;; look for effect of chunking
)


(defn subsets-eduction [coll]
  (let [v (vec coll)
        n (count v)]
    (eduction
     (map (fn [i]
            (->> v
                 (keep-indexed (fn [idx item]
                                 (when (bit-test i idx)
                                   item)))
                 set
                 add-deadweight)))
     (range (bit-shift-left 1 n)))))

(comment
(cmm.trace/trace-var #'subsets-eduction)

(cmm.trace/with-relative-usage
  (count (filter matches? (subsets-eduction (range 2 9)))))

;; but filter doesn't know how to process the eductions so it makes a seq with lots of memory
;; so it's better to use reduce or transduce to get items one at a time

(cmm.trace/with-relative-usage
  (reduce #(if (matches? %2) (inc %1) %1) 0 (subsets-eduction (range 2 9))))

)

;;; SEM comments:  but it's awkward to implement the counting with explicit incs.  Makes me
;;; think we need a count/filter combo to hide this implementation detail

(defn count-when [pred coll]
  (reduce #(if (pred %2) (inc %1) %1) 0 coll))



(comment

(cmm.trace/with-relative-usage
  (count-when matches? (subsets-eduction (range 2 9))))

)

