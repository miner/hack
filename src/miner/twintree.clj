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

;;; non-Baxter, also reverse
(def kpi [3 1 4 2])

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



;;; From my googling, other info on Baxter permutations:
;;; https://www.alfonsobeato.net/tag/baxter-permutations/
;;;
;;; Baxter permutations can be defined by using the generalized pattern avoidance
;;; framework3. The way it works is that a sequence is Baxter if there is no subsequence of
;;; 4 elements that you can find in it that matches one of the generalized patterns 3-14-2
;;; and 2-41-3.  [See vincular patterns below for dashed notation.  See link for cross
;;; references.]





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


;;; https://en.wikipedia.org/wiki/Permutation_pattern
;;;
;;; Permutations patterns are loose ordering patterns of single digits, normally with no
;;; adjacent constraint (so anything can be between the matches).  pattern 213 means there
;;; are some digits x, y, z in that relative order where the values are arranged such
;;; that (y < x < z).  They are not literal matches with the pattern digits, just same
;;; ordering, and we don't care if other digits are in between.

;;; 32415 contains 213 five ways: 3··15, ··415, 32··5, 324··, and ·2·15.
;;; 51342 avoids 213 -- no subsequence of three digits has the ordering implied by pattern
;;; 213.

;;; A dashed pattern (or vincular pattern) can be used when adjacency is required.  The
;;; dashes are like wilcards, otherwise adjacency is required.
;;;
;;; 314265 has two copies of the dashed pattern 2-31-4, given by the entries 3426 and 3425
;;;
;;; SEM: The pattern digit 1 means the smallest value, 4 means the greatest of that
;;; subordering.  Notice that you can leave out a bunch of digits in the source permutation
;;; (literal) when matching dashes, but in this case the third and first smallest digits of
;;; the match must also be adjacent in the original permutation.

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


;;; will overflow for large inputs but I don't care
;;; n items, taking k each
(defn count-combos [n k]
  (quot (reduce * 1 (range (inc (- n k)) (inc n)))
        (reduce * (range 1 (inc k)))))



(defn index-combinations [n k]
  (let [incvk (fn [v]
                (loop [v v dmx n]
                  (when-not (zero? (count v))
                    (let [d (inc (peek v))]
                      (cond (>= d dmx) (recur (pop v) (dec dmx))
                            (= dmx n) (conj (pop v) d)
                            :else (into (pop v) (range d (+ (inc d) (- n dmx)))))))))]
    (loop [res [(vec (range k))]]
      (if-let [pv (incvk (peek res))]
        (recur (conj res pv))
        res))))



(defn index-combinations-WORKS [n k]
  (let [incvk (fn [v]
                (let [nv (loop [v v]
                           (if (empty? v)
                             nil
                             (let [d (inc (peek v))]
                               (if (< d (- n (- k (count v))))
                                 (conj (pop v) d)
                                 (recur (pop v))))))]
                  (when nv
                    (loop [nv nv]
                      (if (= (count nv) k)
                        nv
                        (recur (conj nv (inc (peek nv)))))))))]
    (loop [res [(vec (range k))]]
      (if-let [pv (incvk (peek res))]
        (recur (conj res pv))
        res))))


;;; all combinations of size WIDTH from the elements of (range N)
;;; eager to be faster, not sure if that's good
#_
(defn index-combinations [n width]
  (let [res [(vec (range width))]]
    (let [top (peek res)
          dig (inc (peek top))]
      (if (< dig n)
        (recur (conj res (conj (pop top) dig)))
        ))))
;;        (let [back(loop [dig (peek top)]
;;        (for [i (range width)])))



;;; this is taking ordered subelements, not the same as combo/selections
(defn subelements [elements n]
  "Returns a sequence of vectors of size N, yielding all the ways to take N items from ELEMENTS in
the same relative order as the given ELEMENTS sequence."
  (let [vel (vec elements)]
    (map #(mapv vel %) (combo/combinations (range (count vel)) n))))

;;; vector dissoc, but not a good idea
#_ (defn vdis1 [v i]
  (reduce-kv (fn [r k v] (if (= k i) r (conj r v))) [] v))

#_ (defn vdis [v i]
  (cond (zero? i) (subvec v 1)
        (= i (dec (count v))) (pop v)
        :else (into (subvec v 0 i) (subvec v (inc i)))))

;;; remaps values in pvec to the canonical digits of the one-based pattern notation
;;; (perm-reduction [12 5 4 6 8])  ==>  [5 2 1 3 4]
(defn perm-reduction [pvec]
  (let [rmp (zipmap (sort (set pvec)) (range 1 (inc (count pvec))))]
    (mapv rmp pvec)))

(defn subp4 [pvec]
  (set (map perm-reduction (subelements pvec 4))))


(defn baxter1? [pvec]
  (let [rp4s (into #{} (map perm-reduction) (subelements pvec 4))]
    (not (or (contains? rp4s [3 1 4 2]) (contains? rp4s [2 4 1 3])))))

(defn baxter2? [pvec]
  (empty? (sequence (comp (map #(mapv pvec %))
                          (map perm-reduction)
                          (filter #(or (= [3 1 4 2] %) (= [2 4 1 3] %)))
                          (take 1))
                    (combo/combinations (range (count pvec)) 4))))

;;; halt-when doesn't work right with sequence and still didn't work for me in a transduce

(defn baxter3? [pvec]
  (empty? (sequence (comp (map #(mapv pvec %))
                          (map perm-reduction)
                          (filter #{[3 1 4 2] [2 4 1 3]})
                          (take 1))
                    (combo/combinations (range (count pvec)) 4))))

(defn baxter31? [pvec]
  (empty? (into nil (comp (map #(mapv pvec %))
                          (map perm-reduction)
                          (filter #{[3 1 4 2] [2 4 1 3]})
                          (take 1))
                    (combo/combinations (range (count pvec)) 4))))


(defn baxter42? [pvec]
  (empty? (into nil (comp (map #(mapv pvec %))
                          (map perm-reduction)
                          (filter #(or (= [3 1 4 2] %) (= [2 4 1 3] %)))
                          (take 1))
                (combo/combinations (range (count pvec)) 4))))


;;; slightly faster
(defn baxter4? [pvec]
  (nil? (into nil (comp (map #(mapv pvec %))
                        (map perm-reduction)
                        (filter #(or (= [3 1 4 2] %) (= [2 4 1 3] %)))
                        (take 1))
              (combo/combinations (range (count pvec)) 4))))


(defn bax-test [bax?]
  (assert (false? (bax? [3 1 4 2])))
  (assert (false? (bax? [2 4 1 3])))
  (assert (= (count (filter bax? (cperms 4))) 22))
  (assert (true? (bax? [4 5 2 1 3 8 6 7])))
  true)




(defn disvec
  "Returns a vector based on the vector V with the indices removed from index START
  (inclusive) to END (exclusive, default START+1)."
  ;; (> end start) (pos? end)
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

#_
(defn XXXXbaxter? [perm]
  (let [tree (rvtree perm)
        twin (rvtree (rseq perm))]

    ))
      




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
          


