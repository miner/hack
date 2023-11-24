(ns miner.vincular
  (:require [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;;; see also baxter.clj and stack_sortable.clj

;;; https://en.wikipedia.org/wiki/Permutation_pattern

;;; Note: one-line "231" is that same as vincular "2-3-1".  By the way, wild before and
;;; after pattern is always implied.  The convention is that patterns are specified as
;;; permutations of [1..N] where the digits specify the < order of the actual integers in
;;; some matching collection.

;;; My code is doing a lot of work by brute force.  There's a good chance I missed something
;;; in the literature that might be faster.  Admittedly, I only scratched the surface on the
;;; published math papers.  My goal was to understand the meaning of the terms and to get a
;;; feel for how things worked.  The math papers have a lot more terminolgy and specific
;;; results for particular patterns.


;;; The vincular notation specifies adjacent elements and uses dash - as wildcard.  The
;;; one-line notation implies wildcards between all elements.

;;; intro paper with good explanation
;;; "Generalized Permutation Patterns — A Short Survey"
;;; by Einar Steingr ́imsson
;;; https://www.semanticscholar.org/reader/7fa8ad2c205738047c0d9a877a480e1f73f04d52

;;; Math schemes for enumerating vincular patterns.  Not so simple as what I'm trying.  
;;; https://faculty.valpo.edu/lpudwell/papers/dashedschemes.pdf

;;; Good paper that has proper definitions and probably lots more stuff to understand,
;;; especially with cyclic permutations which I have not even considered.
;;;
;;; https://arxiv.org/pdf/2107.12353.pdf

;;; Paper extending to barred patterns
;;; https://arxiv.org/pdf/1301.6096.pdf

;;; Deeper theorems about kinds of permutations patterns and constructions.
;;; https://www.semanticscholar.org/reader/7c36bc81f544ffb3100ab03c3e97757ad1c0c6c4

#_
(require '[clojure.math.combinatorics :as mc])




;;; converts 231 or "231" to [2 3 1]
(defn one-line-pattern [pat]
  (if (vector? pat)
    pat
    (mapv #(- (long %) (long \0)) (str pat))))


;;; FIXME: this assumes canonical "reduced" one-line pattern.  Probably better to sort like
;;; vincular approach.

;;; This version handles case of pat possibly being within larger pattern (not exact size match)
;;; only handle "one-line" notation, not vincular
(defn one-line-pattern-fn [pat]
  (let [p (one-line-pattern pat)
        cnt (count p)
        ;; _ (assert (and (< cnt 10) (every? #(<= 1 % 9) p))
        ;;     (str "pattern-fn expects 1..9+, failed: " pat))
        pm (zipmap (map dec p) (range))]
    (println "pattern-fn " pm)
    (case cnt
      0 (assert (pos? cnt) "Empty pattern")
      1 (fn [xv] (pos? (count xv)))
      (fn [xv]
        (and (>= (count xv) cnt)
             (some (fn [q] (transduce (map #(nth q (get pm %)))
                                      (fn ([r x] (if (< r x) x (reduced -1))) ([r] (pos? r)))
                                      -1
                                      (range cnt)))
                   (mc/combinations xv cnt)))))))


;;; trying to generalize for vincular patterns as vector of adjpat

;;; returns vector of adjacency vectors [[2 3] [1 4]]  for vincular pattern "23-14"
(defn vincular-pattern [pat]
  (if (vector? pat)
    pat
    (mapv (fn [a] (mapv #(- (long %) (long \0)) a)) (str/split (str pat) #"-"))))




;; you actually need to leave enough room between them, not just <
;; 6 1 3
;; 2 0 1

;;; better idea to check the gaps
;;;  reord [[offset width] ...]
;;; returns fn that checks order matchs adjpat against v at index i
;;;   also check width to allow others to possibly fit in.
;;;   If test succeeds, returns original i, otherwise nil
;;; For convenience, no-arg returns original adjpat -- not sure that's useful but good for
;;; debugging.

;;; handles single adjacency pattern, good preliminary test for allocating adjacent chunks
;;; during search for vincular match.

(defn apat-fn [adjpat]
  (let [cnt (count adjpat)]
    (case cnt
      0 (assert (pos? cnt) "Empty adjpat")
      1 (fn ([] adjpat) ([v i] i))
      2 (let [[a b] adjpat]
          (if (< a b)
            (fn ([] adjpat) ([v i] (when (<= (+ (v i) (- b a)) (v (inc i))) i)))
            (fn ([] adjpat) ([v i] (when (>= (v i) (+ (- a b) (v (inc i)))) i)))))
      ;; 3 or more -- maybe you should optimize three arg with six cases?
      (let [sortas (sort adjpat)
            reord (mapv vector
                        (map first (sort-by peek (map-indexed vector adjpat)))
                        (into [0] (map - (rest sortas) sortas)))]
        ;;(println "apat-fn" reord)
        (fn ([] adjpat)
          ([v i]
           (when (reduce (fn [r [j w]]
                           (let [x (nth v (+ i j))]
                             ;;(println "af" r x w)
                             (if (<= (+ r w) x) x (reduced nil))))
                         -1
                         reord)
             i)))))))


;;; [Extracted from above, but not used directly]

;;; for adjpat we want the width between elements
;;; input [A ...] returns [offset width] within that adjacency
(defn apat-reord [adjpat]
  (let [cnt (count adjpat)]
    (case cnt
      0 (assert (pos? cnt) "Empty adjpat")
      1 [[0 0]]
      2 (let [[a b] adjpat]
          (if (< a b) [[0 0] [1 (- b a)]] [[1 (- a b)] [0 0]]))
      ;; 3 or more -- maybe you should optimize three arg with six cases?  Not now.
      (let [sortas (sort adjpat)
            reord (mapv vector
                        (map first (sort-by peek (map-indexed vector adjpat)))
                        (into [0] (map - (rest sortas) sortas)))]
        ;;(println "apat-reord1" reord)
        reord))))


;;; Note: apat reord is different format from vinc reord, tricky!



;;; input [A ...]  returns [[A Ith offset] ...]
;;; ith index (from original vinc pat), offset is order within adjacency.
;;; Leading A lets it sort correctly with multiple adjpats so we can mix and get the overall order.

(defn vinc-apat-reord [adjpat i]
  (let [cnt (count adjpat)]
    (case cnt
      0 (assert (pos? cnt) "Empty adjpat")
      1 [(into adjpat [i 0])]
      2 (let [[a b] adjpat]
          (if (< a b) [[a i 0] [b i 1]] [[b i 1] [a i 0]]))
      ;; 3 or more -- maybe you should optimize three arg with six cases?  Not now.
      (let [reord (sort (map-indexed (fn [j a] [a i j]) adjpat))]
        (println "vinc-apat-reord" adjpat i "=>" reord)
        reord))))


;;; FIXME -- you shouldn't actually use the vinc-apat-reord with single adjpat as the adjpat-fn
;;; test covers the single case better.

(defn vinc-reord [pat]
  (sort (sequence (mapcat vinc-apat-reord) pat (range))))


;;; FIXME: could optimize for cnt=1, no need to mix
#_
(defn vinc-pat-fn [pat]
  (let [cnt (count pat)
        apv (mapv apat-fn pat)
        cntv (mapv count pat)
        ctot (reduce + 0 cntv)  ;; could be same as (peek (vec starts))
        starts (reductions + 0 (pop cntv))
        endsps  (reductions - ctot (pop cntv))
        ;; apatv (mapv apat-fn pat)
        reordv (mapv vinc-reord (rest (reductions conj [] pat)))]
    (assert (pos? cnt) "Empty pat")
    (println "vinc-pat-fn" reordv)
    (fn ([] pat)
      ([v]
       (let [len (count v)
             ijkv (mapv (fn [ap st en]
                          (let [apf (apat-fn ap)]
                            (filter #(apf v %) (range st (- len en)))))
                        pat starts endsps)]
         ;; FIXME (some (fn [& ijk]
         (reduce (fn [r [_A i off]]
                   (let [x (v (+ i off))]
                     (if (< r x) x (reduced nil))))
                 -1
                 reordv))))))

(defn vinc-fn [reord]
  (case (count reord)
    0 (assert (pos? reord) "Empty reord")
    1 (fn [v i] i)
    (fn [v i]
      (when (reduce (fn [r [_A i off]]
                      (let [x (v (+ i off))]
                        (if (< r x) x (reduced nil))))
                    -1
                    reord)
        i))))


(defn vincpf [pat]
  (let [cnt (count pat)
        apv (mapv apat-fn pat)
        cntv (mapv count pat)
        ctot (reduce + 0 cntv)  ;; could be same as (peek (vec starts))
        ;; don't need startv???
        startv (vec (reductions + 0 (pop cntv)))
        endspv  (vec (reductions - ctot (pop cntv)))
        ;; apatv (mapv apat-fn pat)
        reordv (mapv vinc-reord (rest (reductions conj [] pat)))
        vpv (mapv vinc-fn reordv)]
    ;;; seem faster to (mapv #(vinc-record (subvec ppp 0 %)) (range 1 (inc (count ppp))))
    (assert (pos? cnt) "Empty pat")
    (println "vincpf" reordv)
    (println "  startv" startv)
    (let [v (into (into [] cat pat) cat pat)
          len (count v)]
      (println " fake v " v " len" len)
      (loop [ijk [0]]
        (let [ith (dec (count ijk))
              i (peek ijk)]
          (cond (nil? i) false
                (>= i (- len (endspv i)))
                      (let [ijk2 (pop ijk)]
                        (when-not (zero? (count ijk2))
                          (conj (pop ijk2) (inc (peek ijk2)))))
                  (not ((apv ith) v i)) (recur (conj (pop ijk) (inc i)))
                  (not ((vpv ith) v i)) (recur (conj (pop ijk) (inc i)))
                  (= ith (dec cnt)) ijk ;; success
                  ;; good so far, add another index
                  :else (recur (conj ijk (+ (peek ijk) (cntv ith))))))))))




(defn WIP-SAVE-vincpf [pat]
  (let [cnt (count pat)
        apv (mapv apat-fn pat)
        cntv (mapv count pat)
        ctot (reduce + 0 cntv)  ;; could be same as (peek (vec starts))
        startv (vec (reductions + 0 (pop cntv)))
        endsps  (reductions - ctot (pop cntv))
        ;; apatv (mapv apat-fn pat)
        reordv (mapv vinc-reord (rest (reductions conj [] pat)))]
    ;;; seem faster to (mapv #(vinc-record (subvec ppp 0 %)) (range 1 (inc (count ppp))))
    (assert (pos? cnt) "Empty pat")
    (println "vincpf" reordv)
    (println "  startv" startv)
    (let [v (into (into [] cat pat) cat pat)]
      (println " fake v " v)
      (let [len (count v)
            ijkv (mapv (fn [ap st en]
                          (let [apf (apat-fn ap)]
                            (filter #(apf v %) (range st (- (inc len) en)))))
                       pat startv endsps)]
        (println " len" len)
        ijkv))))

;;; startv is the min indices [A B C]
;;; first check apv for each
;;; if A fails inc A and try again until good A
;;;   then B similarly, plus check vinc-AB until good B
;;;   then C similarly, plus check full vinc (ABC) until good C --> success
;;;   or out of C, then backtrack to inc B again
;;;   same backtrack to inc C again

;;; backtrack = inc previous P and reset Q to (init-Q P) based on widths

;;; might be simpler to init with [0] and add new index as you go (+ (width q) p)



;;; FIXME: NO don't do this.  You need to iterate from the init [i j k] and advance
;;; according to pat widths

(defn lazy-ijks [ijkv cntv]
  (let [spaces (into [0] (pop cntv))]
    (filter #(apply <= (map + % spaces))
            (apply mc/cartesian-product ijkv))))



;;; reord is flat index remap
;;; [[1  0]  [2  7  3  5]  [4  6]]
;;; [1  0    2  7  3  5     4  6]
;;;  3  4   10 11 12 13     20 21

;;; [[a b] [c d e] [f g]]



;;; FIXME:  how do you wire a reord perm function?  Can you synthesize a fn that reords by args?
;;; Without having to remap at runtime.


;;; kind of like mc/cartesian-product but filtered for the coontainment of the total length
;;; of the pattern.

;;; cnt a2 b1 c3  total=6
;;; xv len 12
;;; a: 0 .. 7 (- 12 5)
;;; b: (+ a 2) ..



;;; need to consider pair-wise patterns (and N-wise as well).  It may be hopeless to try the
;;; other adjpat if the two are antagonistic.  Just disqualify immediately.  You can build
;;; out a multi-adjpat (or partial vinc) in the cnt order, largest first.  Mayve only this
;;; stack of tests, not necessarily worth the single adjpat for second, third, etc.

;;; BUGGY NOT FINISHED
(defn BUGGY-vincular-pattern-fn [pat]
  (let [p (vincular-pattern pat)
        adjcnt (count p)
        flatp (into [] cat pat)
        cnt (count flatp)
        apfs (mapv apat-fn p)
        reord (mapv first (sort-by peek (map-indexed vector flatp)))]
    (println "vincular-pattern-fn " reord)
    (fn [xv]
      (and (>= (count xv) cnt)
           (some (fn [iv] (transduce (map #(nth xv (peek %)))
                                    (fn ([r x] (if (< r x) x (reduced -1)))
                                      ([r] (pos? r)))
                                    -1
                                    (sort (map vector reord iv))))
                 ;; FIXME better if lazy ijks
                 (ijks xv p apfs))))))




;;; working example
;; [[4 3] [2] [1 5]]
;; cnt 2   1   2   =   5

;;; experimenting for vincular generation
(defn sample2 []
  (let [pv [[4 3] [2] [1 6 5]]
        cntv (mapv count pv)
        spacev (vec (reductions - (reduce + 0 cntv) (pop cntv)))
        v (vec (range 10 21))
        len (count v)
        maxv (mapv #(- (inc len) %) spacev)]
    ;; (println "pv" pv ", v" v ", max xyz" xmax ymax zmax)
    (for [x (range 0 (maxv 0))
          y (range (+ x (cntv 0)) (maxv 1))
          z (range (+ y (cntv 1)) (maxv 2))]
      ;; (let [_ (println "xyz" x y z)]
      (concat (subvec v x (+ x (cntv 0)))
              (subvec v y (+ y (cntv 1)))
              (subvec v z (+ z (cntv 2)))))))

;;; would like to have a runtime `for` that reduces over a collection of nested endpoints or
;;; intervals.


;;; Think about sliding windows
;;; iterate through "space in front"
;;; try widest first
;;; probably need "state" map with chosen indices
;;; test fn can decode from pv and indicies


(defn sample []
  (let [pv [[4 3] [2] [1 6 5]]
        v (vec (range 10 21))
        cntv (mapv count pv)
        init (reduce (fn [r pp]
                       (let [cnt (count r)]
                         (into r (range cnt (+ cnt (count pp))))))
                     []
                     pv)]
    init))

    
        


#_ (require '[clojure.math.combinatorics :as mc])
            

            

(defn canonical-perm [v]
  (let [remap (zipmap (sort v) (range 1 (inc (count v))))]
    (mapv remap v)))


(defn WAScontains231? [v]
  ;; v is permuation vector of 1..N when N is count
  (some #(= (canonical-perm %) [2 3 1]) (mc/combinations v 3)))


(defn cbax? [v]
  (let [p3142? (vincular-pattern-fn "3-14-2")
        p2413? (vincular-pattern-fn "2-41-3")]
    (not (or (p3142? v) (p2413? v)))))

;;; ----------------------------------------------------------------------
;;; older junk

(defn subv2? [pat i v]
  (let [cnt (count pat)
        pm (zipmap (range) pat)]
    (println "subv " pm)
    (mapv v (map #(+ % i) (sort-by pm (range cnt))))))

;;; reord for adjpat, return adj test fn which checks partial order for that block
;;; returns good index or nil.
(defn adjpat-fn [adjpat]
  (let [cnt (count adjpat)
        reord (mapv peek (sort (map-indexed (fn [i p] (vector p i)) adjpat)))]
    (assert (pos? cnt) "Empty adjpat")
    (fn [v i]
      (transduce (map #(nth v (+ i %)))
                 (fn ([r x] (if (< r x) x (reduced nil)))
                   ([r] (when r i)))
                 -1
                 reord))))


