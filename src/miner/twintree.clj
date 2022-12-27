(ns miner.twintree
  (:require [clojure.math.combinatorics :as combo]))

;; Knuth Christmas Tree lecture 2023
;; https://www.youtube.com/watch?v=zg6YRqT4Duo
;; https://news.ycombinator.com/item?id=34128140

;; article: https://thenewstack.io/donald-knuths-2022-christmas-tree-lecture-is-about-trees/

;; TwinTree
;; turn random order values into binary tree by tree insertion (left less, right greater)
;; also do it in reverse order to create twin
;; such that every node has left child in one tree and right child in one tree


;; Baxter permutations
;; p[k] = l iff p[l] = k
;; =>  q[p[l]] = l

;; has no "bad" configurations,
;; q[k] < l, p[l] > k, p[l+1], q[k+1] > l
;; and similar swapping pqkl
;; no bad small/bigs sequences
;; 3 1 4 2 is a non-Baxter permutation, also 2 4 1 3 (reverse),
;; other 4 number permutations are Baxter.
;; Knuth calls the "Pi mutation" good quick test

;; Baxter 1...m concat with Baxter m+1...n always Baxter
;; also reverse Baxter is Baxter
;; and some other good properties



;;; Floor plans
;;; decomposition of rectangles into rectangles
;;; tatami condition = no four rectangles share a corner point,
;;; four kinds of T-junctions (different directions)

;;; can rearrange x/y coords and redraw rectangles and get "equivalent" plan
;;; (different shapes but similar relations of bounds, above, below, etc.)

;;; flattening rules based on junctions types creates canonical arrangement
;;; useful for comparing similar floor plans

;;; canonical and anti-canonical flattening map to Baxter permutations



;;; Knuth's programs
;;; https://www-cs-faculty.stanford.edu/~knuth/programs.html
;;; look for twintree






;;;; borrowed from my miner/permutations.clj

(defn cperms [n]
  (combo/permutations (range n)))

;; somewhat faster than (c/permuations (range n)) -- but eager, not lazy
(defn range-perms
  "Returns an eager sequence of vectors representing the permutations of the half-open
  range [0, N)."
  [n]
  {:pre [(not (neg? n))]}
  (reduce (fn [vs cnt]
            (reduce (fn [acc vvv]
                      (reduce-kv (fn [r i x] (conj r (assoc vvv i cnt cnt x)))
                                 (conj acc (conj vvv cnt))
                                 vvv))
                    ()
                    vs))
          (list [])
          (range n)))


;;; This is just binary tree stuff.  Maybe deserved own file.

;;; SEM considering binary tree inserts in Clojure.  Immutability requires path copying of
;;; nested vectors or maps,
;;; which seems like a bit of work.

;;; natural node nested:  [VAL LEFT RIGHT]
;;; where LEFT and RIGHT are trees (or nil)
;;; or map {:val VAL :left LEFT :right RIGHT} -- not implemented here

;;; assuming all int vals, and no duplicates (although that should be OK for now)

;;; SEM: note that the order of insertion is not necessarily the same the inorder traversal.

;;; After testing, the vector approach is faster.  And simpler.

;;; Recursive vector approach is much faster.  Uses stack but we don't expect it to get too
;;; deep.  (Follows similar pattern to clojure/assoc-in which is recursive.)

;;; FASTEST and simplest, recursive nested vectors [VAL LEFT RIGHT] or nil
(defn rvinsert [vtree val]
  (if (nil? vtree)
    [val nil nil]
    (let [[node left right] vtree]
      (if (< val node)
        [node (rvinsert left val) right]
        [node left (rvinsert right val)]))))

(defn rvtree [values]
  (reduce rvinsert nil values))

(defn rvinorder [vtree]
  (loop [res [] stack [vtree]]
    (if (empty? stack)
      res
      (let [[val left right] (peek stack)]
        (recur (conj res val)
               ;; note: order matters below
               (cond-> (pop stack)
                 right (conj right)
                 left (conj left)))))))

;;; works but slow.  Maybe good if you need lazy.
(defn rvinorder4 [vtree]
  (let [rvchildren (fn [node]
                     (let [[val left right] node]
                       (cond-> []
                         left (conj left)
                         right (conj right))))]
    (map #(nth % 0) (tree-seq some? rvchildren vtree))))


;;; ----------------------------------------------------------------------

;;; Tried to make it tail-recursive with a stack of path choices, but had to use assoc-in at
;;; the end, which is recursive anyway.  Makes it slower than the natural recursive approach.
(defn vinsert [vtree val]
  (if (nil? vtree)
    [val nil nil]
    (loop [path [] [node left right] vtree]
      (cond (nil? node) (assoc-in vtree path [val nil nil])
            (< val node) (recur (conj path 1) left)
            :else (recur (conj path 2) right)))))

(defn vtree [values]
  (reduce vinsert nil values))

;;; rvinorder is slightly faster but similar idea
(defn vinorder [vtree]
  (loop [stack [vtree] res []]
    (if (empty? stack)
      res
      (let [[val left right] (peek stack)]
        (if (nil? val)
          (recur (pop stack) res)
          (recur (conj (pop stack) right left) (conj res val)))))))




;;; Tried map of nodes and recursive lookups so you can do "flatter" updates.
;;;
;;; assuming unique values! you can do lookups by VAL to get LEFT and RIGHT.  I think it
;;; might be convenient to have the VAL in the node but maybe not necessary.
;;; flat map {::root VAL VAL [VAL LEFT RIGHT]
;;; Works but slower and more complicated.

(defn minsert
  ([mtree val]
   (cond (empty? mtree) {::root val val [nil nil]}
         (contains? mtree val) (throw (ex-info "Duplicate value" {:duplicate val}))
         :else (minsert mtree val (::root mtree))))
  ([mtree val node]
   (let [[left right] (mtree node)]
     (if (< val node)
       (if (nil? left)
         (assoc mtree node [val right] val [nil nil])
         (recur mtree val left))
       (if (nil? right)
         (assoc mtree node [left val] val [nil nil])
         (recur mtree val right))))))

(defn mtree [values]
  (reduce minsert nil values))

(defn minorder [mtree]
  (loop [stack [(::root mtree)] res []]
    (if (empty? stack)
      res
      (if-let [head (peek stack)]
        (recur (into (pop stack) (rseq (mtree head))) (conj res head))
        (recur (pop stack) res)))))
          


