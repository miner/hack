(ns miner.baxter
  (:require [clojure.math.combinatorics :as combo]
            [clojure.data.int-map :as im]))

;; see also twintree.clj for Knuth's Baxter test.

;; Knuth Christmas Tree lecture 2023
;; about correspondence between twintrees, Baxter permuations and floor plans
;;
;; https://www.youtube.com/watch?v=zg6YRqT4Duo
;; https://news.ycombinator.com/item?id=34128140
;; article: https://thenewstack.io/donald-knuths-2022-christmas-tree-lecture-is-about-trees/

;; A Baxter permutation is on that has has no "bad" configurations.  That is, avoids
;; particular patterns.

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
;;; framework. The way it works is that a sequence is Baxter if there is no subsequence of
;;; 4 elements that you can find in it that matches one of the generalized patterns 3-14-2
;;; and 2-41-3.  [See vincular patterns below for dashed notation.  See link for cross
;;; references.]

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
;;; The pattern digit 1 means the smallest value, 4 means the greatest of that subordering.
;;; Notice that you can leave out a bunch of digits in the source permutation
;;; (literal) when matching dashes, but in this case the third and first smallest digits of
;;; the match must also be adjacent in the original permutation.

;;; The other pattern notation is called oneline notation (reference?), in which no
;;; adjacency is implied.  So 213 (oneline) is the same as 2-1-3 (vincular).  By the way,
;;; all the notations for patterns are concerned with finding some subsequence that matches
;;; the pattern, not the whole sequence at once.  You can think of it as implying wildcards
;;; at the beginning and end of the given pattern.

;;; The literature seems to be one-based for indexing.  That can cause some translation
;;; problems into Clojure vectors using zero-based indexing.  Beware of bugs.

;;; another source on Baxter permutations:
;;; https://www.wikiwand.com/en/Baxter_permutation


;;; SEM New Idea:  try using im/dense-int-set instead of bits.  More like Clojure sets.


;; not faster than bbax?, about 2x
(defn imbax? [v]
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [beforev (reduce (fn [bv i] (conj bv (conj (peek bv) i)))
                              [(conj (im/dense-int-set) (nth v 0))]
                              (subvec v 1))
              afterv (vec (reduce (fn [bv i] (conj bv (conj (peek bv) i)))
                                  (list (conj (im/dense-int-set) (peek v)))
                                  (rseq (pop v))))]
          ;; (println "beforev" beforev)
          ;; (println "afterv" afterv)
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (if (> v2 v1)
                   ;; looking for 3-14-2
                   (let [low (first (im/range (afterv (+ 2 i)) (inc v1) (dec v2)))]
                     (when low
                       (first (im/range (beforev (dec i)) (inc low) (dec v2)))))

                   ;; looking for 2-41-3
                   (let [low (first (im/range (beforev (dec i)) (inc v2) (dec v1)))]
                     (when low
                       (first (im/range (afterv (+ 2 i)) (inc low) (dec v1))))) ))))

           (range 1 (- cnt 2)))))))

;; maybe clearer?
(defn imbax2? [v]
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [beforev (reduce (fn [bv i] (conj bv (conj (peek bv) i)))
                              [(conj (im/dense-int-set) (nth v 0))]
                              (subvec v 1))
              afterv (reduce (fn [bv i] (conj bv (disj (peek bv) i)))
                             [(peek beforev)]
                             (pop v))]
          ;; (println "beforev" beforev)
          ;; (println "afterv" afterv)
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (if (> v2 v1)
                   ;; looking for 3-14-2
                   (let [low (first (im/range (afterv (+ 2 i)) (inc v1) (dec v2)))]
                     (when low
                       (first (im/range (beforev (dec i)) (inc low) (dec v2)))))

                   ;; looking for 2-41-3
                   (let [low (first (im/range (beforev (dec i)) (inc v2) (dec v1)))]
                     (when low
                       (first (im/range (afterv (+ 2 i)) (inc low) (dec v1))))) ))))

           (range 1 (- cnt 2)))))))

#_ (require '[clojure.data.int-map :as im])



(defn cperms [n]
  (combo/permutations (range 1 (inc n))))

;;;; see also my miner/permutations.clj  for range-perms, etc.

;;; combo/count-combinations is better in general.
;;; will overflow for large inputs but I don't care
;;; n items, taking k each
(defn count-combos [n k]
  (quot (reduce * 1 (range (inc (- n k)) (inc n)))
        (reduce * (range 1 (inc k)))))


;;; New idea: assume v of 1..N with N<64. Convert to bits and mark long per index each way.
;;; Essentially a set of bits of everything before or after an index in the original vector.
;;; Should be fast finding whatever on the outsides by bit manipulations.

;;; Pair at i [v1 v2] establish range.
;;; if (> v2 v1), looking for 3-14-2 scenario.
;;; low is lowest bit above v1 in the mask-after (matching 2)
;;; high is higest bit below v2 in the mask-before (matching 3)
;;; found iff (low not zero) and highest-bit of high > lowest-bit of low
;;; if (< v2 v1), looking for 2-41-3 so invert logic for finding high and low but same idea.


;;; Should be faster but I'm not measuring it???  about same as baxter? for long test,
;;; slower for test-bax.
(defn bbax? [v]
  ;; (assert (< (count v) 62))
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [mask-before (reduce (fn [bv i] (conj bv (bit-set (peek bv) i)))
                                  [(bit-set 0 (nth v 0))]
                                  (subvec v 1))
              mask-after (reduce (fn [bv i] (conj bv (bit-clear (peek bv) i)))
                                 [(peek mask-before)]
                                 (pop v))]
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (if (> v2 v1)
                   ;; looking for 3-14-2
                   (let [low (bit-and (bit-shift-left -1 v1) (mask-after (+ 2 i)))]
                     (when-not (zero? low)
                       (> (bit-and (dec (bit-set 0 v2)) (mask-before (dec i)))
                          (bit-and low (- low)))))
                   ;; looking for 2-41-3
                   (let [low (bit-and (bit-shift-left -1 v2) (mask-before (dec i)))]
                     (when-not (zero? low)
                       (> (bit-and (dec (bit-set 0 v1)) (mask-after (+ 2 i)))
                          (bit-and low (- low)))))))))
           (range 1 (- cnt 2)))))))

;;; note (bit-and x (- x)) ==> lowest one bit of x, same as (Long/lowestOneBit x)



;;; not faster, not simpler
(defn bbax8? [v]
  ;; (assert (< (count v) 62))
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [mask-before (reduce (fn [bv i] (conj bv (bit-set (peek bv) i)))
                                  [(bit-set 0 (nth v 0))]
                                  (subvec v 1))
              mask-after (reduce (fn [bv i] (conj bv (bit-clear (peek bv) i)))
                                 [(peek mask-before)]
                                 (pop v))]
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (let [gt (> v2 v1)
                       a (if gt v1 v2)
                       b (if gt v2 v1)
                       mask-2 (if gt (mask-after (+ 2 i)) (mask-before (dec i)))
                       mask-3 (if gt (mask-before (dec i)) (mask-after (+ 2 i)))
                       low (bit-and (bit-shift-left -1 a) mask-2)]
                   (when-not (zero? low)
                     (> (bit-and (dec (bit-set 0 b)) mask-3) (Long/lowestOneBit low)))))))
           (range 1 (- cnt 2)))))))

(defn bbax82? [v]
  ;; (assert (< (count v) 62))
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [mask-before (reduce (fn [bv i] (conj bv (bit-set (peek bv) i)))
                                  [(bit-set 0 (nth v 0))]
                                  (subvec v 1))
              mask-after (reduce (fn [bv i] (conj bv (bit-clear (peek bv) i)))
                                 [(peek mask-before)]
                                 (pop v))]
          (not-any?
           (fn [i]
             (let [vi (v i)
                   vi2 (v (inc i))
                   diff (- vi2 vi)]
               ;; check if there's room between v1 and v2
               (when (> (abs diff) 2)
                 (let [a (if (pos? diff) vi vi2)
                       b (if (pos? diff) vi2 vi)
                       mask2 (if (pos? diff) (mask-after (+ 2 i)) (mask-before (dec i)))
                       mask3 (if (pos? diff) (mask-before (dec i)) (mask-after (+ 2 i)))
                       low (bit-and (bit-shift-left -1 a) mask2)]
                   (when-not (zero? low)
                     ;; High-bit is "3", low bit is "2"
                     (> (bit-and (dec (bit-set 0 b)) mask3) (bit-and low (- low))))))))
           (range 1 (- cnt 2)))))))





(defn bbax3? [v]
  ;; (assert (< (count v) 62))
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [mask-before (reduce (fn [bv i] (conj bv (bit-set (peek bv) i)))
                                  [(bit-set 0 (nth v 0))]
                                  (subvec v 1))
              mask-after (reduce (fn [bv i] (conj bv (bit-clear (peek bv) i)))
                                 [(peek mask-before)]
                                 (pop v))]
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (if (> v2 v1)
                   ;; looking for 3-14-2
                   (let [low (bit-and (bit-shift-left -1 v1) (mask-after (+ 2 i)))]
                     (when-not (zero? low)
                       (> (Long/highestOneBit (bit-and (dec (bit-set 0 v2))
                                                       (mask-before (dec i))))
                          (Long/lowestOneBit low))))
                   ;; looking for 2-41-3
                   (let [low (bit-and (bit-shift-left -1 v2) (mask-before (dec i)))]
                     (when-not (zero? low)
                       (> (Long/highestOneBit (bit-and (dec (bit-set 0 v1))
                                                       (mask-after (+ 2 i))))
                          (Long/lowestOneBit low))))))))
           (range 1 (- cnt 2)))))))




;; bit hack assuming 2s complement representation
(defn low-one-bit [i]
  (bit-and i (- i)))

;;; exclusive of i bit
(defn bits-below [i]
  (dec (bit-set 0 i)))

;;; exclusive of i bit
(defn bits-above [i]
  (bit-shift-left -1 (inc i)))

(defn bits-btw [i j]
  ;; (assert (< i j))
  ;; (assert (pos? j))
  (bit-shift-left (dec (bit-set 0 (- j (inc i)))) (inc i)))

(defn bits-btw2 [i j]
  ;; (assert (< i j))
  ;; (assert (pos? j))
  (bit-shift-left (dec (bit-shift-left 1 (- j (inc i)))) (inc i)))
             

;; 3-14-2
;; 
;; mask-before>>   1.....4   <<mask-after
;; 2 is least maft within 1..4       bit-and btw14 (maft i+2)
;;       or lowest bit in maft above v1
;; 3 is anything within 2..4
;;       or highest bit in mbef below v4
;; 
;; [high bit left]
;;      4.....1
;;   m a f   2   t t t
;;    m b 3 e   f 



(defn bbax2? [v]
  ;; (assert (< (count v) 62))
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [bits-btw (fn [i j]
                         ;; (assert (< i j))
                         ;; (assert (pos? j))
                         (bit-shift-left (dec (bit-set 0 (- j (inc i)))) (inc i)))
              mask-before (reduce (fn [bv i] (conj bv (bit-set (peek bv) i)))
                                  [(bit-set 0 (nth v 0))]
                                  (subvec v 1))
              mask-after (reduce (fn [bv i] (conj bv (bit-clear (peek bv) i)))
                                 [(peek mask-before)]
                                 (pop v))]
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (if (> v2 v1)
                   ;; looking for 3-14-2
                   (let [mask (bits-btw v1 v2)
                         low  (bit-and mask (mask-after (+ 2 i)))]
                     (when-not (zero? low)
                       (> (Long/highestOneBit (bit-and mask (mask-before (dec i))))
                          (Long/lowestOneBit low))))
                   ;; looking for 2-41-3
                   (let [mask (bits-btw v2 v1)
                         low (bit-and mask (mask-before (dec i)))]
                     (when-not (zero? low)
                       (> (Long/highestOneBit (bit-and mask (mask-after (+ 2 i))))
                          (Long/lowestOneBit low))))))))
           (range 1 (- cnt 2)))))))

;;; SAVE THIS VERSION
(defn bbax1? [v]
  (let [cnt (count v)]
    (or (< cnt 4)
        (let [bits-btw (fn [i j]
                         ;; (assert (< i j))
                         ;; (assert (pos? j))
                         (bit-shift-left (dec (bit-set 0 (- j (inc i)))) (inc i)))
              lowest-bit (fn [i] (when-not (zero? i) (Long/numberOfTrailingZeros i)))
              mask-before (reduce (fn [bv i] (conj bv (bit-set (peek bv) i)))
                                  [(bit-set 0 (nth v 0))]
                                  (subvec v 1))
              mask-after (reduce (fn [bv i] (conj bv (bit-clear (peek bv) i)))
                                 [(peek mask-before)]
                                 (pop v))]
          (not-any?
           (fn [i]
             (let [v1 (v i)
                   v2 (v (inc i))]
               ;; check if there's room between v1 and v2
               (when (> (abs (- v2 v1)) 2)
                 (if (> v2 v1)
                   ;; looking for 3-14-2
                   (when-let [after (lowest-bit (bit-and (bits-btw v1 v2) (mask-after (+ 2 i))))]
                     (not (zero? (bit-and (bits-btw after v2) (mask-before (dec i))))))
                   ;; looking for 2-41-3
                   (when-let [before (lowest-bit (bit-and (bits-btw v2 v1) (mask-before (dec i))))]
                     (not (zero? (bit-and (bits-btw before v1) (mask-after (+ 2 i))))))))))
           (range 1 (- cnt 2)))))))




;;; current approach for bax -- run through i=2..n look at middle adjacent (v i) (v (inc i))
;;; decide < > then scan before and after appropriately for 3-14-2 and 2-41-3.
;;; Any single match disqualifies.

;;; v is a vector permutation of 1..N, small positive ints. By convention, zero is not
;;; included.  An element appears only once.  The literature is 1-based so you have to be
;;; careful about endpoints.  Clojure indexing is zero-based.

;;; much faster than baxter11  10x
(defn was-baxter? [v]
  (let [cnt (count v)]
    (or (< cnt 4)
        (not-any?
         (fn [i]
           (let [v1 (v i)
                 v2 (v (inc i))]
             ;; check if there's room between v1 and v2
             (when (> (abs (- v2 v1)) 2)
               (if (> v2 v1)
                 ;; looking for 3-14-2
                 (let [after (reduce min v2 (filter #(< v1 % v2) (subvec v (+ 2 i))))]
                   (when (< after v2)
                     (some #(< after % v2) (subvec v 0 i))))
                 ;; looking for 2-41-3
                 (let [before (reduce min v1 (filter #(< v2 % v1) (subvec v 0 i)))]
                   (when (< before v1)
                     (some #(< before % v1) (subvec v (+ 2 i)))))))))
         (range 1 (- cnt 2))))))



;;; The new champion!  Transducers for the win.

(defn baxter? [v]
  (let [cnt (count v)]
    (or (< cnt 4)
        (not-any?
         (fn [i]
           ;; i is the index of the second element B in pattern A-BC-D.
           ;; BC are adjacent, A and D can be anywhere before or after.
           (let [b (v i)
                 c (v (inc i))]
             ;; check if there's room between B and C
             (when (> (abs (- c b)) 2)
               (let [before (subvec v 0 i)
                     after (subvec v (+ 2 i))]
                 (if (> c b)
                   ;; looking for 3-14-2
                   (transduce (filter #(< b % c))
                              (fn ([x y] (min x y))
                                ([d] (when (< d c)
                                       (some (fn [a] (< d a c)) before))))
                              c
                              after)
                   ;; looking for 2-41-3
                   (transduce (filter #(< c % b))
                              (fn ([x y] (min x y))
                                ([a] (when (< a b)
                                       (some (fn [d] (< a d b)) after))))
                              b 
                              before))))))
         (range 1 (- cnt 2))))))


;;; Unimplemented ideas: maybe worth considering size of partitions when deciding which way
;;; to look first.
;;; I thought it might be faster to look middle out, but it wasn't.  Maybe overhead of
;;; middle-out isn't worth it.

;;; borrowed from miner/halfbaked.clj
(defn interleave-all
  "Returns a lazy seq of the first item in each collection, then the second, etc.  If one 
collection ends, continues to interleave the others.  Naturally, you should take care with
infinite sequences."
  ([] nil)
  ([c] (lazy-seq c))

  ([c1 c2]
     (lazy-seq
      (cond (not (seq c1)) c2
            (not (seq c2)) c1
            :else (conj (interleave-all (rest c1) (rest c2)) (first c2) (first c1)))))

  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (keep seq (conj colls c2 c1))]
        (concat (map first ss) (apply interleave-all (map rest ss)))))))

(defn center-out [n]
  (let [h (quot n 2)]
    (interleave-all (range h (- n 2)) (range (dec h) 0 -1))))

;; SLOWER
(defn bax6? [v]
  (let [cnt (count v)]
    (or (< cnt 4)
        (not-any? (fn [i]
                    (let [v1 (v i)
                          v2 (v (inc i))]
                      ;; check if there's room between v1 and v2
                      (when (> (abs (- v2 v1)) 2)
                        (if (> v2 v1)
                          ;; looking for 3-14-2
                          (let [after (reduce min v2 (filter #(< v1 % v2) (subvec v (+ 2 i))))]
                            (when (< after v2)
                              (some #(< after % v2) (subvec v 0 i))))
                          ;; looking for 2-41-3
                          (let [before (reduce min v1 (filter #(< v2 % v1) (subvec v 0 i)))]
                            (when (< before v1)
                              (some #(< before % v1) (subvec v (+ 2 i)))))))))
                  (center-out cnt)))))


(defn test-bax [bax?]
  (assert (bax? [1 2 3 4]))
  (assert (not (bax? [3 1 4 2])))
  (assert (not (bax? [2 4 1 3])))
  (assert (= (count (filter bax? (cperms 5))) 92))
  true)

;;; BUG -- should only test with actual permuations 1..N unique

(defn bug-test-bax [bax?]
  (assert (not (bax? [3 1 4 2])))
  (assert (not (bax? [2 4 1 3])))
  ;; jenny
  (assert (bax? [8 6 7 5 3 0 9]))
  (assert (not (bax? [4 0 8 8 6 7 5 3 0 9])))
  ;; lost
  (assert (bax? [4 8 15 16 23 42]))
  (assert (bax? [4 5 2 1 3 8 6 7]))
  (assert (= (count (filter bax? (cperms 4))) 22))
  true)


;; baxter? still wins
;;(assert (= (count (filter bax? (cperms 9))) 58202))

(def hk (vec (range 1 1001)))
(def h3k (into (vec (range 1 1501)) (range 3000 1500 -1)))

;;; maybe ztw4 beats baxter? for large inputs -- just barely
(defn test-bax-long [bax?]
  (assert (test-bax bax?))
  (assert (bax? hk))
  (assert (bax? h3k))
  true)

;;; Number of Baxter permutations for size N = 0...9
;;; 0, 1, 2, 6, 22, 92, 422, 2074, 10754, 58202

;;; This is OEIS sequence A001181.
;;; https://oeis.org/A001181


;;; There's a formula for the count of Baxter permutations of size N.
;;; For any positive integer

;;; "N choose K" or binomial coefficient
;;; C(n, k)= n!/[k!(n-k)!]

;;; Beware overflow for larger values of N > 20.  That's fine for our needs now.  You can
;;; switch to *' and friends for auto-promotion to bigints.
(defn fact [n]
  (reduce * 1 (range 2 (inc n))))

(defn nck [n k]
  (/ (fact n) (* (fact k) (fact (- n k)))))

(defn count-baxter [n]
  (let [n1 (inc n)]
    (reduce + 0 (map (fn [k]
                       (/ (* (nck n1 (dec k)) (nck n1 k) (nck n1 (inc k)))
                          (* n1 (nck n1 2))))
                     (range 1 n1)))))

;; slightly faster with local memoize but not much faster.  Better to external memoize
(defn count-baxter2 [n]
  (let [n1 (inc n)
        fact (memoize (fn [n] (reduce * 1 (range 2 (inc n)))))
         nck (memoize (fn [n k] (/ (fact n) (* (fact k) (fact (- n k))))))]
    (reduce + 0 (map (fn [k]
                       (/ (* (nck n1 (dec k)) (nck n1 k) (nck n1 (inc k)))
                          (* n1 (nck n1 2))))
                     (range 1 n1)))))

(defn count-baxter3 [n]
  (let [n1 (inc n)
        fact (memoize fact)
         nck (memoize nck)]
    (reduce + 0 (map (fn [k]
                       (/ (* (nck n1 (dec k)) (nck n1 k) (nck n1 (inc k)))
                          (* n1 (nck n1 2))))
                     (range 1 n1)))))

(def mfact (memoize fact))

(defn nckm [n k]
  (/ (mfact n) (* (mfact k) (mfact (- n k)))))

(def mnck (memoize nckm))

;; faster with all memoized, but not sure if it's worth it
(defn count-baxter4 [n]
  (let [n1 (inc n)]
    (reduce + 0 (map (fn [k]
                       (/ (* (mnck n1 (dec k)) (mnck n1 k) (mnck n1 (inc k)))
                          (* n1 (mnck n1 2))))
                     (range 1 n1)))))




;;; Knuth's first example input
(def xkex [7 1 5 3 2 8 4 6])

;;; Jenny is Baxter  (but not sequential)
(def xjenny [8 6 7 5 3 0 9])

;;; Lost is Baxter (but not sequential)
(def xlost [4 8 15 16 23 42])

;;; non-Baxter, also reverse
(def xkpi [3 1 4 2])






;;; ----------------------------------------------------------------------


;;; Losers


;;; The baxter permutation avoids subelement patterns of 3-14-2 and 2-41-3.  We consider all
;;; the combinations of three indicies (given in the natural ascending order).  The middle
;;; must be extendible to two elements (adjacent), which means the second can't be right next to
;;; the third.  After that, we just need to check for the two excluded < orderings implied
;;; by the patterns.  (But it turns out to be faster to just walk the middles and search the
;;; ends as done if my final baxter?)

;;; This could be a bit faster if you check adjacent (v j) and j+1 for enough space.
;;; However, my final baxter? is much faster anyway.  Consider this fairly literal and
;;; workable.

;;; Keep this as the simplest definition.  Good for testing and base timing.
(defn baxter-canonical? [v]
  (not-any? (fn [[i j k]]
              (let [a (v i) b (v j) c (v (inc j)) d (v k)]
                ;;  3 1 4 2  or  2 4 1 3
                (or (< b d a c) (< c a d b))))
            (combo/combinations (range (count v)) 3)))


;;; like (combo/combinations (range n) k) but a bit faster. Eager, not lazy.  Not really
;;; good enough to replace combo/combinations but I tried.
(defn index-combinations [n k]
  (let [seed (vec (range k))]
    (loop [v seed dmx n res [seed]]
      (if (zero? (count v))
        res
        (let [d (inc (peek v))]
          (cond (>= d dmx) (recur (pop v) (dec dmx) res)
                (= dmx n) (let [v (conj (pop v) d)]
                            (recur v n (conj res v)))
                :else (let [v (into (pop v) (range d (+ (inc d) (- n dmx))))]
                        (recur v n (conj res v)))))))))

;;; slower.  Special case on normal inc (= dmx n) is worth it
(defn index-combinations2 [n k]
  (let [seed (vec (range k))]
    (loop [v seed dmx n res [seed]]
      (if (zero? (count v))
        res
        (let [d (inc (peek v))]
          (if (>= d dmx)
            (recur (pop v) (dec dmx) res)
            (let [v (into (pop v) (range d (+ (inc d) (- n dmx))))]
              (recur v n (conj res v)))))))))



;;; This turns out not to be so useful as Baxter testing requires a bit of expansion
;;;
;;; this is taking ordered subelements, not the same as combo/selections
(defn subelements [elements n]
  "Returns a sequence of vectors of size N, yielding all the ways to take N items from ELEMENTS in
the same relative order as the given ELEMENTS sequence."
  (let [vel (vec elements)]
    (map #(mapv vel %) (combo/combinations (range (count vel)) n))))


;;; not really needed anymore
;;; remaps values in pvec to the canonical digits of the one-based pattern notation
;;; (perm-reduction [12 5 4 6 8])  ==>  [5 2 1 3 4]
(defn perm-reduction [pvec]
  (let [rmp (zipmap (sort (set pvec)) (range 1 (inc (count pvec))))]
    (mapv rmp pvec)))




;;; It is significantly slower to do the pattern reduction technique.  I leave this comment
;;; to explain what it was trying to do.
;;; It's a bit faster to do the mapping and pattern reduction and comparison all in one function.
;;; baxter-excluded returns just the "copy" or selection of v that violates the baxter
;;; avoidance rule.  Ultimately, we only care about the two avoidance patterns so we don't
;;; have to dedupe the "reduction" pattern which would not give the normal result if
;;; duplicates are the selection.  Anything with duplicates is not going to match the
;;; avoidance patterns.  Note that we have to take 3 indicies and reject those that don't
;;; allow a two element expansion of the second term.  The original approach was buggy (and
;;; slower) in trying to take all 4 slots, ignoring the adjacency requirement for the middle
;;; match.

;; slower
(defn bax8? [v]
  (not-any? (fn [[i j k]]
              (when (not= (inc j) k)
                (let [r [(v i) (v j) (v (inc j)) (v k)]
                      rmp (zipmap (sort r) (range 1 5))
                      redp (mapv rmp r)]
                  (or (= redp [3 1 4 2]) (= redp [2 4 1 3])))))
            (combo/combinations (range (count v)) 3)))





(defn bax7? [v]
  (let [baxter-excluded (fn [[i j k]]
                          (when (not= (inc j) k)
                            (let [r [(v i) (v j) (v (inc j)) (v k)]
                                  rmp (zipmap (sort r) (range 1 5))
                                  redp (mapv rmp r)]
                              (when (or (= redp [3 1 4 2]) (= redp [2 4 1 3]))
                                r))))]
    (nil? (into nil (comp (keep #(baxter-excluded %))
                          (take 1))
                (combo/combinations (range (count v)) 3))))) 

(defn exp-triple [[i j k]]
  (when (not= (inc j) k)
    [i j (inc j) k]))

;;; original idea but it's slower than the final.  Remapping was much slower than looking at
;;; the baxter < ordering of the candidate 4 elements.
(defn baxter-WORKS? [pvec]
  (empty? (sequence (comp (keep exp-triple)
                          (map #(mapv pvec %))
                          (map perm-reduction)
                          (filter #(or (= [3 1 4 2] %) (= [2 4 1 3] %)))
                          (take 1))
                    (combo/combinations (range (count pvec)) 3))))

;;; halt-when doesn't work right with sequence and still didn't work for me in a transduce



;;; returns the subsequence that matches the baxter avoidance pattern -- this is, the
;;; example that fails the baxter criteria.  Could be faster if you check (v j), j+1 for
;;; enough room first.
(defn baxter-excluded [v [i j k]]
  (when (not= (inc j) k)
    (let [a (v i) b (v j) c (v (inc j)) d (v k)]
      ;; 3142  or  2413
      (when (or (< b d a c) (< c a d b))
        [a b c d]))))

;;; returns sequence of subelements that fail the baxter avoidance
;;; useful for debugging
(defn baxter-fail [pvec]
  (seq (keep #(baxter-excluded pvec %) (combo/combinations (range (count pvec)) 3))))



;;; Old idea
(defn baxter-excluded-WORKS [v [i j k]]
  (when (not= (inc j) k)
    (let [r [(v i) (v j) (v (inc j)) (v k)]
          rmp (zipmap (sort r) (range 1 5))
          redp (mapv rmp r)]
      (when (or (= redp [3 1 4 2]) (= redp [2 4 1 3]))
        r))))

(defn bax5? [pvec]
  (not-any? #(baxter-excluded pvec %) (combo/combinations (range (count pvec)) 3)))

