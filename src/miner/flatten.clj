(ns miner.flatten
  (:require [clojure.core.reducers :as r]))



;; Usually, you want sequential? (for flattening nested sequences, lists, and vectors).
;; If you just want to deal with lists and lazy-seqs, use seq? (vectors will not be flattened).
;; If you want to flatten maps, sets, and all kinds of collections, you could use coll?, but that
;; seems like an unusual situation.

;; flat-seq is about twice as fast as clojure.core/flatten.
;; If you're going to use reduce on the result, see the new clojure.core.reducers library
;; for a much faster way to flatten.

;; guts of flat inspired by a post by Mark Engleberg  mark.engelberg@gmail.com
(defn flat-seq
  "Like `clojure.core/flatten` but better, stronger, faster.  Takes an optional
   predicate `pred` that returns true if an element should be flattened.  If unspecified,
   the default pred is sequential?.  Returns a single, flat, lazy sequence.  If
   `x` is nil, nil is returned.  If `(pred x)` is falsey, returns `(list x)` so it's
   always safe to treat the result as a seq."
  {:static true}
  ([xs] (flat-seq sequential? xs))
  ([pred xs]
     (let [flat1 (fn flat [pred xs]
                   (if-let [xs (seq xs)]
                     (let [y (first xs)
                           ys (rest xs)]
                       (if (pred y)
                         (if-let [y (seq y)]
                           (recur pred (cons (first y) (cons (rest y) ys)))
                           ;; empty list case
                           (recur pred ys))
                         (lazy-seq (cons y (flat pred ys)))))
                     ()))]
       (cond (nil? xs) nil
             (pred xs) (flat1 pred xs)
             :else (list xs)))))


;; Consider using reducers instead of this
(defn eager-flatten
  "not lazy, but much faster than clojure.core/flatten."
  [coll] 
  (loop [cs coll, result []]
    (cond (sequential? (first cs)) (recur (concat (first cs) (rest cs)) result)
          (empty? cs) (seq result)
          :else (recur (rest cs) (conj result (first cs))))))

(defn eflat
  "not lazy, but much faster than clojure.core/flatten."
  [coll] 
  (loop [cs coll, result []]
    (cond (sequential? (first cs)) (recur (concat (first cs) (rest cs)) result)
          (empty? cs) result
          :else (recur (rest cs) (conj result (first cs))))))

;; but it's slower than eager-flatten!
(defn eflatten
  "not lazy, but much faster than clojure.core/flatten."
  [coll] 
  (loop [cs coll, result []]
    (cond (empty? cs) (seq result)
          (vector? (first cs)) (recur (into (first cs) (rest cs)) result)
          (sequential? (first cs)) (recur (into (rest cs) (first cs)) result)
          :else (recur (rest cs) (conj result (first cs))))))



;; reducers is fast
(defn rflatten [coll]
  (into [] (r/flatten coll)))

;; recursive version derived from my keypaths
;; As fast as rlatten!  pretty good
(defn fltt
  ([coll]
   (if (coll? coll)
     (persistent! (fltt coll (transient [])))
     (if (nil? coll) [] (vector coll))))
  ([coll result]
   (reduce (fn [r x]
             (if (coll? x)
               (fltt x r)
               (conj! r x)))
           result
           coll)))


(def nested [99 (reductions conj [] (range 100)) [11 [12 [13] 14 [15]]]])
(def flatsum (reduce + 0 (flatten nested)))



(defn smoke [f]
  (assert (= (reduce + 0 (f nested)) flatsum))
  true)

(defn deep1 [n]
  (reduce (fn [r _] (list r)) '(1) (range n)))

(def big 7000)
(def big-nest (take big (iterate list 1)))

(def nest1 (deep1 big))

;; contrived, extreme case
(defn smokebig [f]
  (assert (= (reduce + 0 (f big-nest)) big))
  true)


;;; https://stackoverflow.com/questions/49832153/clojure-beginner-implement-flatten-trouble-with-let
;; solution on stackoverflow
(defn flat 
  ([xs] (flat xs []))
  ([xs acc]
    (if (empty? xs) acc
      (if (coll? (first xs))
        (flat (rest xs) (flat (first xs) acc))
        (recur (rest xs) (conj acc (first xs)))))))

;; coll? is arguably wrong since you usually don't want to flatten a map or set
;; sequential? is probably a better choice.

;; flat and sflat will fail with smoke10 because the nesting is too deep
(defn sflat 
  ([xs] (sflat xs []))
  ([xs acc]
    (if (empty? xs) acc
      (if (sequential? (first xs))
        (sflat (rest xs) (sflat (first xs) acc))
        (recur (rest xs) (conj acc (first xs)))))))

;; another stackoverflow answer
(defn flat2 [x] (if (coll? x) (mapcat flat2 x) [x]))

;; SEM trans variant
(defn tflat [xs]
  (into [] (mapcat (fn [x] (if (sequential? x) (tflat x) (list x)))) xs))

