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

(defn cperms1 [n]
  (combo/permutations (range 1 (inc n))))

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

;;; Lots of good comments in the Knuth source, especially about rules for generating new
;;; Baxter permutations from existing ones.  (Reminds me of the juggling site-swap rules.)


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

;;; assume values are uniquely 1..N (the usual for permuations, otherwise remap them)

;;; arrays of size (n+1) -- slot 0 holds root, empty=0 otherwise ref to node index

  ;;; assuming [1..N] no zeros
(defn arr-tree [values]
  (let [mx (reduce max 0 values)
        larr (int-array (inc mx))
        rarr (int-array (inc mx))]
    (assert (pos? mx))
    (assert (pos? (first values)))
    (aset-int larr 0 (first values))
    (aset-int rarr 0 (first values))
    (doseq [x (rest values)]
      (assert (pos? x))
      (loop [node (aget larr 0)]
        (if (< x node)
          (let [left (aget larr node)]
            (if (zero? left)
              (aset-int larr node x)
              (recur left)))
          (let [right (aget rarr node)]
            (if (zero? right)
              (aset-int rarr node x)
              (recur right))))))
    [larr rarr]))

(defn ztree [values]
  (let [root (first values)]
    (loop [xs (rest values)
           node root
           mleft {}
           mright {}]
      (if-let [x (first xs)]
        (if (< x node)
          (if-let [left (mleft node)]
            (recur xs left mleft mright)
            (recur (rest xs) root (assoc mleft node x) mright))
          (if-let [right (mright node)]
            (recur xs right mleft mright)
            (recur (rest xs) root mleft (assoc mright node x))))
        [mleft mright]))))

;; for debugging
(defn mpv [m]
  (when (seq m)
    (let [mx (reduce max 0 (vals m))]
      (when (pos? mx)
        (map #(m % 0) (range 0 (inc mx)))))))

;; slightly faster than ztwin and simpler
(defn ztw2 [pv]
  (let [[l0 r0] (ztree pv)
        [l1 r1] (ztree (rseq pv))
        cnt (count pv)
        parl (zipmap (vals l1) (keys l1))
        parr (zipmap (vals r1) (keys r1))]
    (loop [t0 (first pv) ps (seq pv) l0 l0 r0 r0 l1 l1 r1 r1]
      (if-not (= t0 (first ps))
        false
        (if-let [i (parl t0)]
          (let [right (r0 t0)]
            (recur (or right (l0 t0))
                   (rest ps)
                   (if right (assoc l0 i (l0 t0)) l0)
                   r0
                   (dissoc l1 i)
                   r1))
          (if-let [i (parr t0)]
            (let [left (l0 t0)]
              (recur (or left (r0 t0))
                     (rest ps)
                     l0
                     (if left (assoc r0 i (r0 t0)) r0)
                     l1
                     (dissoc r1 i)))
            (nil? (next ps))))))))
              


;; works.  Clojure data structures.  2x slower than arr-twin.
;; more or less Knuth's code structure
(defn ztwin [pv]
  (let [[l0 r0] (ztree pv)
        [l1 r1] (ztree (rseq pv))
        cnt (count pv)
        par (loop [k 1 par {}]
              (if (> k cnt)
                par
                (recur (inc k)
                       (cond-> par
                         (contains? l1 k) (assoc (l1 k) k)
                         (contains? r1 k) (assoc (r1 k) (- k))))))]
    (loop [t0 (first pv) ps (seq pv) l0 l0 r0 r0 l1 l1 r1 r1]
      (let [i (par t0)]
        (cond (nil? i) (= t0 (first ps))
              (not= t0 (first ps)) false
              ;; (empty? ps) true
              (pos? i) (let [right (r0 t0)]
                         (recur (or right (l0 t0))
                                (rest ps)
                                (if right (assoc l0 i (l0 t0)) l0)
                                r0
                                (dissoc l1 i)
                                r1))
              :else (let [i (- i)
                          left (l0 t0)]
                      (recur (or left (r0 t0))
                             (rest ps)
                             l0
                             (if left (assoc r0 i (r0 t0)) r0)
                             l1
                             (dissoc r1 i))))))))




                      
                                


;;; why is aset ^ints faster than aset-int ????  Maybe doing some other coercions silently?
;;; I don't know.

;;; WORKS but seems slow, about 3x baxter?
;;; require [1..N] no zeros

(defn arr-twin [pv]
  (let [[l0 r0] (arr-tree pv)
        [l1 r1] (arr-tree (rseq pv))
        cnt (count pv)
        par (int-array (inc cnt))]
    (doseq [k (range 1 (inc cnt))]
      (let [node (aget ^ints l1 k)]
        (when-not (zero? node) (aset ^ints par ^int node ^int k)))
      (let [node (aget ^ints r1 k)]
        (when-not (zero? node) (aset ^ints par ^int node ^int (- k)))))
    (loop [t0 (int (nth pv 0)) ps (seq pv)]
      (let [i (aget ^ints par t0)]
        (cond (empty? ps) true
              (not= t0 (first ps)) false
              ;;(zero? i) true
              (pos? i) (let [right (aget ^ints r0 t0)]
                         (aset ^ints l1 i 0)
                         (when-not (zero? right) (aset ^ints l0 i (aget ^ints l0 t0)))
                         (recur (int (if (zero? right) (aget ^ints l0 t0) right))
                                (rest ps)))
              :else (let [i (- i)
                          left (aget ^ints l0 t0)]
                      (aset ^ints r1 i 0)
                      (when-not (zero? left) (aset ^ints r0 i (aget ^ints r0 t0)))
                      (recur (int (if (zero? left) (aget ^ints r0 t0) left))
                             (rest ps))))))))


;;    [(vec l0) (vec r0) (vec l1) (vec r1)]))






;;; maybe worth holding onto parent as v3 ???
(defn pvinsert
  ([vtree val] (pvinsert vtree val nil))
  ([vtree val parent]
  (if (nil? vtree)
    [val nil nil parent]
    (let [[node left right] vtree]
      (if (< val node)
        [node (pvinsert left val node) right parent]
        [node left (pvinsert right val node) parent])))))

(defn pvtree [values]
  (reduce pvinsert nil values))


;;; collect parent for each node, need to keep track of left/right status
;;; could try transients
(defn parent-map [vtree]
  (loop [stack [vtree] pm {}]
    (if (empty? stack)
      pm
      (let [[node left right] (peek stack)]
        (recur (cond-> (pop stack)
                 (some? right) (conj right)
                 (some? left) (conj left))
               (cond-> pm
                 (some? left) (assoc-in [(nth left 0) :parent-left] node)
                 (some? right) (assoc-in [(nth right 0) :parent-right] node)))))))


(defn rvpmap [vtree]
  (loop [stack [vtree] pm {}]
    (if (empty? stack)
      pm
      (let [[node left right] (peek stack)]
        (recur (cond-> (pop stack)
                 (some? right) (conj right)
                 (some? left) (conj left))
               (cond-> pm
                 (some? left) (assoc (nth left 0) node)
                 (some? right) (assoc (nth right 0) node)))))))


;; not any faster with transient vector.  A bit faster with transient map.
(defn peek! [tv]
  (nth tv (dec (count tv)) nil))

;;; WRONG IDEA
(defn tparent-map [vtree]
  (loop [stack (transient [vtree]) pm (transient {})]
    (if (empty? stack)
      (persistent! pm)
      (let [[node left right] (peek! stack)]
        (recur (cond-> (pop! stack)
                 (some? right) (conj! right)
                 (some? left) (conj! left))
               (cond-> pm
                 (some? left) (assoc! (nth left 0) node)
                 (some? right) (assoc! (nth right 0) node)))))))



;;; better idea - use different keys for :left :right or :left1 :right1
(defn lr-tree
  ([vtree] (lr-tree vtree {}))
  ([vtree lrmap] (lr-tree vtree lrmap :left :right))
  ([vtree lrmap lkey rkey]
   (loop [stack [vtree] res lrmap]
     (if (empty? stack)
       res
       (let [[node left right] (peek stack)]
         (recur (cond-> (pop stack)
                  (some? right) (conj right)
                  (some? left) (conj left))
                (cond-> res
                  (some? left) (assoc-in [node lkey] (nth left 0))
                  (some? right) (assoc-in [node rkey] (nth right 0)))))))))



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




;;; Slow.  Probably too much nested maps.  Should try Java arrays.  Or at least, less
;;; nesting.  Separate colls for each prop l0,r0,l1,r1,pars
;;; See arr-twin for java array impl. Much faster than this, but still not good compared to
;;; my baxter?

;; Note :parent is from twin tree1, not tree0.
(defn twin-baxter? [v]
   (let [tree (rvtree v)
         twin (rvtree (rseq v))
         full-state (lr-tree twin (lr-tree tree (parent-map twin)) :left1 :right1)]
     (loop [ps (seq v)  state full-state  t0 (first ps)]
       (if-not (= (first ps) t0)
         false
         ;;; do algo as it assigns t0, and check that first ps matches result
         (if-let [i (get-in state [t0 :parent-left])]
           (let [tright (get-in state [t0 :right])
                 tnext (or tright (get-in state [t0 :left]))]
           (recur (rest ps)
                  (-> state
                      (assoc-in [i :left1] nil)
                      (cond-> tright (assoc-in [i :left] (get-in state [t0 :left]))))
                  tnext))
           (if-let [i (get-in state [t0 :parent-right])]
             (let [tleft (get-in state [t0 :left])
                   tnext (or tleft (get-in state [t0 :left]))]
               (recur (rest ps)
                      (-> state
                          (assoc-in [i :right1] nil)
                          (cond-> tleft (assoc-in [i :right] (get-in state [t0 :right]))))
                      tnext))
             true))))))
 



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
          







