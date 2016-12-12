(ns miner.selsort)

;; NOTE: this is not a good sort method.  But the original poster wanted so advice on how to
;; do better with with the same algorithm.  And, in general, how to deal with input data
;; that should shrink on each pass.  I wanted to show how to use peek, pop, assoc and
;; reduce-kv with vectors.

;; Along the way, I discovered a bug with subvectors -- they don't support reduce-kv.  The
;; quick work-around is to add APersistentVector$SubVector to the IKVReduce protocol but
;; it's not as fast as it could be.  Better to implement something like kvreduecrange to
;; PersistentVecotor, like how iteratorRange works.

;;----------------------------------------------------------------------
;; mailing list question about selection-sort

(defn smallest [xs]
   "Searches for smallest element and returns it's value and position"
  (reduce #(let [[e im ci] %]
            (if (<= %2 e)
              [%2 ci (inc ci)]
              [e im (inc ci)])) [(first xs) 0 0] xs))

(defn selection-sort [s]
  "Selection sort itself :)"
  (loop [xs s acc []]
    (if (seq xs)
      (let [[x i] (smallest xs)]
        (recur (concat (take i xs) (drop (inc i) xs)) (conj acc x))) ; <--
      acc)))

;;----------------------------------------------------------------------
;; another suggestion

(defn smallest2 [xs]
  (->> xs (map-indexed vector) (sort-by second) first))

(defn selection-sort2 [s]
  (->> (iterate
        (fn [[acc xs]]
          (let [[i x] (smallest2 xs)
                [l r] (split-at i xs)]
            [(conj acc x) (concat l (rest r))]))
        [[] s])
       (drop-while (comp not-empty second))
       ffirst))

;; ----------------------------------------------------------------------


;; my suggestions (unpublished)
;; 0. Not a good problem: use built-in sort, or Java mutable arrays instead
;; 1. Use vectors, not any collection -- indexed? matters for this algorithm

(defn selsort [coll]
  (loop [v (vec coll) sv []]
    (if (seq v)
      (let [pi (dec (count v))
            px (peek v)
            [i0 x0] (reduce-kv (fn [r i x] (if (< x (peek r)) [i x] r))
                               [pi px]
                               (pop v))]
        (recur (pop (assoc v i0 px)) (conj sv x0)))
      sv)))



(defn selsort1-WORKS [coll]
  (loop [v (vec coll) sv []]
    (if (seq v)
      (let [pi (dec (count v))
            pk (peek v)
            [imin vmin] (reduce-kv (fn [r i x] (if (<= x (peek r)) [i x] r))
                                  [pi pk]
                                  (pop v))]
        (recur (pop (assoc v imin pk)) (conj sv vmin)))
      sv)))

;; Slightly slower, the "clever" check for i = pi maybe slows it down
;; Beware, there was a bug with assoc-ing into the end of v1 (even after the pop) which
;; caused an infinite loop.
(defn selsort11 [coll]
  (loop [v (vec coll) sv []]
    (if (seq v)
      (let [v1 (pop v)
            px (peek v)
            pi (count v1)
            [i x] (reduce-kv (fn [r i x] (if (<= x (peek r)) [i x] r))
                                  [pi px]
                                  v1)]
        (recur (if (= i pi) v1 (assoc v1 i px)) (conj sv x)))
      sv)))


;; maybe shouldn't reuse same vector
;; could have separate result -- note the sense of the compare op depending on building from
;; greatest at end vs. smallest at front.

;; much slower, maybe the subvec is a bad idea
(defn selsort2 [coll]
  (loop [v (vec coll) n (count v)]
    (if (zero? n)
      v
      (let [pi (dec n)
            pk (nth v pi)
            [imax vmax] (reduce-kv (fn [r i x] (if (> x (peek r)) [i x] r))
                                  [pi pk]
                                  (subvec v 0 n))]
        (recur (assoc v pi vmax imax pk) pi)))))

;; BUG: reduce-kv not supported on a subvec


;; Attempt to implement protocol

;; works but should be faster with a native Java implementation in APersistentVector.java
(extend-type clojure.lang.APersistentVector$SubVector
  clojure.core.protocols/IKVReduce
  (kv-reduce [subv f init]
    (.kvreduce ^clojure.lang.PersistentVector (into [] subv) f init)))

;; converting to vector is generally faster than faking it with (map-indexed vector) even
;; with transduce

;; BUT what about memory usage?  GC?

;; seems faster to just bite the bullet and copy into a vector.
;; Note: (vec subv) will not make a real PersistentVector, it just leaves the original subv.
;; (into [] subv) is necessary.  (The change to `vec` was introduced in Clojure 1.7.)

(defn redkv [f init subv]
  (.kvreduce ^clojure.lang.PersistentVector (into [] subv) f init))

;; notice the args are different orders between kv-reduce and reduce-kv
(defn slow-redkv [f init subv]
  (reduce (fn [ret [k v]] (f ret k v))
          init
          (map-indexed vector subv)))

;; better
(defn tredkv1 [f init subv]
  (transduce (map-indexed vector)
             (fn ([ret] ret) ([ret [k v]] (f ret k v)))
             init
             subv))



(defn tredkv2 [f init subv]
  (transduce (map-indexed list)
             (fn ([ret] ret) ([ret kv] (apply f ret kv)))
             init
             subv))


(defn slowkv [res k v]
  (+ res (* k (inc v)) (quot (dec k) 3)))

