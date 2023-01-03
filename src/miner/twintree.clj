(ns miner.twintree
  (:require [clojure.math.combinatorics :as combo]))


#_
(require '[clojure.math.combinatorics :as c])

;; Knuth Christmas Tree lecture 2023
;; https://www.youtube.com/watch?v=zg6YRqT4Duo
;; https://news.ycombinator.com/item?id=34128140

;; article: https://thenewstack.io/donald-knuths-2022-christmas-tree-lecture-is-about-trees/

;; TwinTree
;; turn random order values into binary tree by tree insertion (left less, right greater)
;; also do it in reverse order to create twin
;; such that every node has left child in exactly one of the twin trees 1 <= k < n
;; also one right child in exactly one tree 1 < k <= n
;; end points only have one child


;;; Knuth's first example input
(def kex [ 7 1 5 3 2 8 4 6])

;;; Jenny is Baxter
(def jenny [8 6 7 5 3 0 9])

;;; Lost is Baxter
(def lost [4 8 15 16 23 42])

;;; non-Baxter, also reverse
(def kpi [3 1 4 2])

;; Baxter permutations
;; p[k] = l iff p[l] = k
;; =>  q[p[l]] = l



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
;;; https://www-cs-faculty.stanford.edu/~knuth/programs/twintree-to-baxter.w





;;; generic, might be useful.  Probably wrong file.
(defn disvec
  "Returns a vector based on the vector V with the indices removed from index START
  (inclusive) to END (exclusive, default START+1)."
  ;; {:pre [(> end start) (pos? end)]
  ([v start] (disvec v start (inc start)))
  ([v start end]
   (cond (zero? start) (subvec v end)
         (= end (count v)) (subvec v 0 start)
         :else (into (subvec v 0 start) (subvec v end)))))



;;; This is just binary tree stuff.  Maybe deserved own file.

;;; Rich Hickey has a gist for a deftype binary TreeNode
;;; https://gist.github.com/richhickey/874276

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

(defn lefts-rights [tree]
  (loop [stack [tree] lefts #{} rights #{}]
    (if (empty? stack)
      [lefts rights]
      (let [[val left right] (peek stack)]
        (cond (and (nil? left) (nil? right)) (recur (pop stack) lefts rights)
              (nil? left) (recur (conj (pop stack) right) lefts
                                 (conj rights val))
              (nil? right) (recur (conj (pop stack) left) (conj lefts val)
                                  rights)
              :else (recur (conj (pop stack) right left)
                           (conj lefts val) (conj rights val)))))))




(defn rvtree->levels [vtree]
  (loop [res [] level [vtree]]
    ;; (println "level" level)
    ;; (println "res" res)
    (if (first (filter some? level))
      (recur (conj res (mapv #(nth % 0 nil) level))
             (into [] (mapcat #(list (nth % 1 nil) (nth % 2 nil))) level))
      res)))

;;; BUGGY -- rewrite from bottom up assigning space to node above, then center in given
;;; space.

(defn print-rvtree [vtree]
  (let [levels (rvtree->levels vtree)
        cnt (count levels)
        width (* 5 (quot (count (peek levels))2))]
    (dotimes [_ (quot width 4)] (print " "))
    (println (first (levels 0)))
    (doseq [i (range 1 cnt)]
      (let [w (quot width (* 4 (count (levels i))))]
        (dotimes [_ w] (print " "))
        (dotimes [j (count (levels i))]
          (when (even? j) (print " "))
          (print (or ((levels i) j) "."))
          (dotimes [_ w] (print " ")))
        (println)))))
    


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
          


