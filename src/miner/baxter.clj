(ns miner.baxter
  (:require [clojure.math.combinatorics :as combo]))

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

;;; The literature seems to be one-based for indexing.  That can cause some translation
;;; problems into Clojure vectors using zero-based indexing.  Beware of bugs.

(defn cperms [n]
  (combo/permutations (range 1 (inc n))))

;;;; see also my miner/permutations.clj  for range-perms, etc.

;;; combo/count-combinations is better in general.
;;; will overflow for large inputs but I don't care
;;; n items, taking k each
(defn count-combos [n k]
  (quot (reduce * 1 (range (inc (- n k)) (inc n)))
        (reduce * (range 1 (inc k)))))


;;; new idea for bax -- run through i=2..n look at middle adjacent (v i) (v (inc i))
;;; decide < > then scan before and after appropriately for 3-14-2 and 2-41-3.
;;; Any single match disqualifies.

;;; v is a vector permutation so you can assume elements appear only once
;;; probably safe to assume open interval [0..n) for size N
;;; The literature is 1-based so you have to be careful about endpoints.

;;; much faster than baxter11  10x
(defn baxter? [v]
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
  (assert (false? (bax? [3 1 4 2])))
  (assert (false? (bax? [2 4 1 3])))
  (assert (= (count (filter bax? (cperms 5))) 92))
  true)

;;; BUG -- should only test with actual permuations 1..N unique

(defn bug-test-bax [bax?]
  (assert (false? (bax? [3 1 4 2])))
  (assert (false? (bax? [2 4 1 3])))
  ;; jenny
  (assert (bax? [8 6 7 5 3 0 9]))
  (assert (not (bax? [4 0 8 8 6 7 5 3 0 9])))
  ;; lost
  (assert (bax? [4 8 15 16 23 42]))
  (assert (true? (bax? [4 5 2 1 3 8 6 7])))
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



;;; Knuth's first example input
(def xkex [ 7 1 5 3 2 8 4 6])

;;; Jenny is Baxter
(def xjenny [8 6 7 5 3 0 9])

;;; Lost is Baxter
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

(defn baxter11? [v]
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

