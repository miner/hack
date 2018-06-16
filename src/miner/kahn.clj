;; Copyright (c) Alan Dipert. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

;;; Original gist:  https://gist.github.com/alandipert/1263783
;;; hacked version by SEM
;;; Kahn algorithm for topological sort
;;; https://en.wikipedia.org/wiki/Topological_sorting

(ns #_ alandipert.kahn
    miner.kahn
  (:require [clojure.set :refer [difference union intersection]]))

;; SEM `without` should just be disj
(defn without
  "Returns set s with x removed."
  [s x] (difference s #{x}))

;; SEM better to do this inline (see ksort)
(defn take-1
  "Returns the pair [element, s'] where s' is set s with element removed."
  [s] {:pre [(not (empty? s))]}
  (let [item (first s)]
    [item (without s item)]))

(defn no-incoming
  "Returns the set of nodes in graph g for which there are no incoming
  edges, where g is a map of nodes to sets of nodes."
  [g]
  (let [nodes (set (keys g))
        have-incoming (apply union (vals g))]
    (difference nodes have-incoming)))

(defn normalize
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (let [have-incoming (apply union (vals g))]
    (reduce #(if (get % %2) % (assoc % %2 #{})) g have-incoming)))

(defn kahn-sort
  "Proposes a topological sort for directed graph g using Kahn's
   algorithm, where g is a map of nodes to sets of nodes. If g is
   cyclic, returns nil."
  ([g]
     (kahn-sort (normalize g) [] (no-incoming g)))
  ([g l s]
     (if (empty? s)
       (when (every? empty? (vals g)) l)
       (let [[n s'] (take-1 s)
             m (g n)
             g' (reduce #(update-in % [n] without %2) g m)]
         (recur g' (conj l n) (union s' (intersection (no-incoming g') m)))))))

;; SEM transient doesn't help much here
(defn noin [g]
  (persistent! (reduce-kv (fn [s _ v] (reduce disj! s v)) (transient (set (keys g))) g)))

(defn noin1 [g]
  (reduce-kv (fn [s _ v] (reduce disj s v)) (set (keys g)) g))

(defn norm
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (let [have-incoming (persistent! (reduce-kv (fn [s _ v] (reduce conj! s v))
                                              (transient #{})
                                               g))]
    (persistent! (reduce #(if (get % %2) % (assoc! % %2 #{})) (transient g) have-incoming))))


(defn norm2
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (persistent!
   (reduce #(if (get % %2) % (assoc! % %2 #{}))
           (transient g)
           (persistent! (reduce-kv (fn [s _ v] (reduce conj! s v)) (transient #{}) g)))))

;; slightly slower than norm2 but simpler.  Also converts seq vals to sets (for more
;; convenient specification).  Arguably, you don't need this, but you also could require
;; explicit empty sets if you want to skip normalization altogether.
(defn norm3
  "Returns g with empty outgoing edges added for nodes with incoming
  edges only.  Example: {:a #{:b}} => {:a #{:b}, :b #{}}"
  [g]
  (reduce #(if-let [v (get % %2)]
             (if (set? v) % (assoc % %2 (set v)))
             (assoc % %2 #{}))
           g
           (reduce-kv (fn [s _ v] (reduce conj s v))  #{} g)))



;;; SEM version
(defn ksort
  "Proposes a topological sort for directed graph g using Kahn's
   algorithm, where g is a map of nodes to sets of nodes. If g is
   cyclic, returns nil."
  ([g]
     (ksort (norm2 g) [] (noin g)))
  ([g l s]
     (if (empty? s)
       (when (every? empty? (vals g)) l)
       (let [n (first s)
             s' (disj s n)
             m (g n)
             g' (reduce #(update % n disj %2) g m)]
         (recur g' (conj l n) (union s' (intersection (noin g') m)))))))


(comment
  (def acyclic-g
    {7 #{11 8}
     5 #{11}
     3 #{8 10}
     11 #{2 9}
     8 #{9}})

  (def cyclic-g
    {7 #{11 8}
     5 #{11}
     3 #{8 10}
     11 #{2 9}
     8 #{9}
     2 #{11}}) ;oops, a cycle!

  (kahn-sort acyclic-g) ;=> [3 5 7 8 10 11 2 9]
  (kahn-sort cyclic-g) ;=> nil

  ;;; SEM actually I get...
  (kahn-sort acyclic-g)  ;=> [7 3 5 11 2 10 8 9]
  ;;; as there can be multiple valid topological sorts.
  ;;; My guess is that Clojure set seq order can vary.

  )
