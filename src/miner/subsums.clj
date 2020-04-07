(ns miner.subsums
  (:require [clojure.math.combinatorics :as mc]))


;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-372-model-change-over-time-with-state-machines/

;; challenge

#_
(require '[clojure.math.combinatorics :as mc])

(defn subset-sums [nums sum]
  (->> nums
       vec
       mc/subsets
       (filter #(zero? (reduce - sum %)))
       (map set)
       set))








(defn choose-indices [cnt choose]
  ;; {:pre [(pos-int? choose) (<= choose cnt)]}
  (loop [i (dec choose) res (map vector (range cnt))]
    (if (zero? i)
      res
      (recur (dec i)
             (mapcat (fn [prev] (map #(conj prev %)
                                     (range (inc (peek prev)) cnt)))
                     res)))))


;; map only while adding,
;; still slower than obvious mc version
(defn subset-sums34 [nums sum]
  (let [v (vec nums)
        cnt (count v)]
    (into (if (zero? sum) [()] [])
          (comp
           (mapcat #(choose-indices cnt %))
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(map v %)))
          (range 1 (inc cnt)))))


;; all sets all the time
(defn subset-sums36 [nums sum]
  (let [v (vec nums)
        cnt (count v)]
    (into (if (zero? sum) #{()} #{})
          (comp
           (mapcat #(choose-indices cnt %))
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(into #{} (map v) %)))
          (range 1 (inc cnt)))))


;; slightly faster than obvious version, much more complicated
;; uses mc/combinations which is faster than my my choose-indices
(defn subset-sums46 [nums sum]
  (let [v (vec nums)
        cnt (count v)]
    (into (if (zero? sum) #{()} #{})
          (comp
           (mapcat #(mc/combinations (range cnt) %))
           (filter #(zero? (reduce (fn [r i] (- r (v i))) sum %)))
           (map #(into #{} (map v) %)))
          (range 1 (inc cnt)))))






(defn subset-sums12 [nums sum]
  (into #{}
        (comp
         (filter #(zero? (reduce - sum %)))
         (map set))
        (mc/subsets (vec nums))))

(defn subset-sums1 [nums sum]
  (set (map set (filter #(= (reduce + 0 %) sum) (mc/subsets (vec nums))))))

;; mc/subsets doesn't work with sets!  Must seq or vec nums input.  Vec is slightly faster.


;; basically the same
(defn subset-sums2 [nums sum]
  (filter #(= (reduce + 0 %) sum) (mc/subsets (vec nums))))

(defn subset-sums3 [nums sum]
  (sequence (filter #(= (reduce + 0 %) sum)) (mc/subsets (vec nums))))

(defn subset-sums4 [nums sum]
  (into #{} (comp (filter #(zero? (reduce - sum %))) (map set)) (mc/subsets (vec nums))))





;; SEM: Note Eric gave two incomplete examples!

(defn smoke-subs
  ([] (smoke-subs subset-sums))
  ([subset-sums]
   (let [set= (fn [a b] (= (set (map set a)) (set (map set b))))]
     (assert (set= (subset-sums #{1 2 3} 0) #{ #{} }))
     (assert (set= (subset-sums #{1 2 3 4 5} 6)  #{ #{1 5} #{2 4} #{1 3 2}} ))
     (assert (set= (subset-sums #{1 2 3 5 6 7} 7)  #{ #{1 6} #{2 5} #{7} }))
     (assert (set= (subset-sums #{0 1 -1} 0) #{ #{} #{0} #{1 -1} #{0 1 -1} }))
     (assert (set= (subset-sums (range 10) 42)
                   #{#{7 1 4 6 2 9 5 8} #{0 7 4 6 3 9 5 8} #{7 4 6 3 9 5 8}
                     #{0 7 1 4 6 2 9 5 8}}))
     true)))









;; mc/subsets doesn't work with sets!  Must seq or vec nums input.  Vec is slightly faster.

;; inside mc/combinations calls distinct, which doesn't work on a set!

;; (distinct #{1 2 3})
;; Error printing return value (UnsupportedOperationException) at
;; clojure.lang.RT/nthFrom (RT.java:991). nth not supported on this type: PersistentHashSet

;; (let [[f :as xs] #{1 2 3}] xs)
;; Execution error (UnsupportedOperationException) at user/eval2925 (REPL:1).
;; nth not supported on this type: PersistentHashSet
