(ns miner.vincular
  (:require [clojure.math.combinatorics :as mc]
            [clojure.data.int-map :as im]
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


;;; NEW IDEA:  try adj pat only for single vinc, otherwise only vinc 2+.  Theory is that
;;; adjpat is not saving much if you still have to can vinc and vinc on 1 is trivial because
;;; adjpat is the real test there.  So eliminated apv and incorporate adjpat 1 into vpv.
;;;   NOT FASTER

;;; I think the real issue is that I need to test longest adjpat first.  That was the
;;; original idea but I punted on the implementation.

;;; "1-2345-6"  seems like 2-block should go first as it is hardest to satisfy




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

;;; faster
(defn unique-pattern? [pat]
  (transduce cat
             (fn ([x] (boolean x))
               ([r x] (if (contains? r x) (reduced false) (conj r x))))
             #{}             
             pat))

(defn uniq-pat? [pat]
  (apply distinct? (sequence cat pat)))

;;; returns vector of adjacency vectors [[2 3] [1 4]]  for vincular pattern "23-14"
(defn vincular-pattern [pat]
  {:pre [(seq pat)] :post [(apply distinct? (sequence cat %))]}
  (if (vector? pat)
    pat
    (mapv (fn [a] (mapv #(- (long %) (long \0)) a)) (str/split pat #"-"))))




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
      1 (fn [v i] i)
      2 (let [[a b] adjpat]
          (if (< a b)
            (fn [v i] (when (<= (+ (v i) (- b a)) (v (inc i))) i))
            (fn [v i] (when (>= (v i) (+ (- a b) (v (inc i)))) i))))
      ;; 3 or more -- maybe you should optimize three arg with six cases?
      (let [sortas (sort adjpat)
            reord (mapv vector
                        (map first (sort-by peek (map-indexed vector adjpat)))
                        (into [0] (map - (rest sortas) sortas)))]
        ;;(println "apat-fn" reord)
        (fn [v i]
          (when (reduce (fn [r [j w]]
                          (let [x (nth v (+ i j))]
                            ;;(println "af" r x w)
                            (if (<= (+ r w) x) x (reduced nil))))
                        -1
                        reord)
            i))))))

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
        ;;(println "vinc-apat-reord" adjpat i "=>" reord)
        reord))))


;;; FIXME -- you shouldn't actually use the vinc-apat-reord with single adjpat as the adjpat-fn
;;; test covers the single case better.

;;; only when (count pat) = 1 so it's just an adjpat
(defn vinc-apat-fn [adjpat]
  (let [cnt (count adjpat)]
    (case cnt
      0 (assert (pos? cnt) "Empty adjpat")
      1 (fn [v ijk] ijk)
      2 (let [[a b] adjpat]
          (if (< a b)
            (fn [v [i]] (when (<= (+ (v i) (- b a)) (v (inc i))) [i]))
            (fn [v [i]] (when (>= (v i) (+ (- a b) (v (inc i)))) [i]))))
      ;; 3 or more -- maybe you should optimize three arg with six cases?
      (let [sortas (sort adjpat)
            reord (mapv vector
                        (map first (sort-by peek (map-indexed vector adjpat)))
                        (into [0] (map - (rest sortas) sortas)))]
        ;;(println "apat-fn" reord)
        (fn [v [i]]
          (when (reduce (fn [r [j w]]
                          (let [x (nth v (+ i j))]
                            ;;(println "af" r x w)
                            (if (<= (+ r w) x) x (reduced nil))))
                        -1
                        reord)
            [i]))))))

(defn vinc-fn [pat]
  (let [reord (sort (sequence (mapcat vinc-apat-reord) pat (range)))]
    (case (count reord)
      0 (assert (pos? reord) "Empty vinc reord")
      1 (fn [v ijk] ijk)  ;; the apat-fn does the real work for single
      (fn [v ijk]
        (when (reduce (fn [r [_A i off]]
                        (let [x (v (+ (ijk i) off))]
                          (if (< r x) x (reduced nil))))
                      -1
                      reord)
          ijk)))))

;;; New idea on integrate apat-vinc.  Sounds like a good idea, but not faster for bax.
(defn vp-fn-in-order [pat]
  ;; (assert (pos? (count pat)) "Empty pat")
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        vpv (into [(vinc-apat-fn (nth pat 0))]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]

    (fn ([] pat)
      ([v]
       (let [len (count v)
             maxv (mapv #(- len %) endsp)]
         (loop [ijk [0]]
           (let [ith (dec (count ijk))
                 i (peek ijk)]
             (cond (nil? i) false
                   (> i (maxv ith))
                       (let [ijk2 (pop ijk)]
                         (recur (when-not (zero? (count ijk2))
                                  (conj (pop ijk2) (inc (peek ijk2))))))
                   (not ((vpv ith) v ijk))   (recur (conj (pop ijk) (inc i)))
                   (= ith (dec cnt))   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijk (+ (peek ijk) (cntv ith))))))))))))


(defn vp-fn [pat]
  ;; (assert (pos? (count pat)) "Empty pat")
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        sorted-apat (sort-by count pat)
        vpv (into [(vinc-apat-fn (nth pat 0))]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]

    (fn ([] pat)
      ([v]
       (let [len (count v)
             maxv (mapv #(- len %) endsp)]
         (loop [ijk [0]]
           (let [ith (dec (count ijk))
                 i (peek ijk)]
             (cond (nil? i) false
                   (> i (maxv ith))
                       (let [ijk2 (pop ijk)]
                         (recur (when-not (zero? (count ijk2))
                                  (conj (pop ijk2) (inc (peek ijk2))))))
                   (not ((vpv ith) v ijk))   (recur (conj (pop ijk) (inc i)))
                   (= ith (dec cnt))   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijk (+ (peek ijk) (cntv ith))))))))))))





;;; it might be better to pre-filter all ijks by apatfn rather than retrying ks mulitple
;;; times.  More bookkeeping but should be faster

;;; do not bother with (vpv 0) as (apv 0) covers -- but actually it's slower to test ith.
;;; Need to replace the vpv 0 with a constantly true fn.  The apv test will happen first so
;;; if that succeeded the vpv would have as well.  BTW, `constantly` produced a slower test
;;; than an explicit fn with the right args.  Might be worth adding explicit args to
;;; constantly???  No, my testing is suspect.  Better to do the obvious thing

(def argv '[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9])

(defn adj-fn [apat]
  (let [cnt (count apat)
        args (subvec argv 0 cnt)
        reo (mapv peek (sort (map vector apat args)))]
    (println apat)
    reo))
;; but also need spacing



(defn vincular-pattern-fn [pat]
  ;;  (assert (pos? (count pat)) "Empty pat")
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        apv (mapv apat-fn pat)
        vpv (into [(constantly true)]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]
    (fn ([] pat)
      ([v]
       (let [len (count v)
             maxv (mapv #(- len %) endsp)]
         (loop [ijk [0]]
           (let [ith (dec (count ijk))
                 i (peek ijk)]
             (cond (nil? i) false
                   (> i (maxv ith))
                       (let [ijk2 (pop ijk)]
                         (recur (when-not (zero? (count ijk2))
                                  (conj (pop ijk2) (inc (peek ijk2))))))
                   (not ((apv ith) v i))   (recur (conj (pop ijk) (inc i)))
                   (not ((vpv ith) v ijk))   (recur (conj (pop ijk) (inc i)))
                   (= ith (dec cnt))   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijk (+ (peek ijk) (cntv ith))))))))))))







;;; New idea:  try keep apat-fn state of testing in long bits.  Mark off failures so you
;;; don't have to try again.  Also need to keep lenght of previously tested.  So that's two
;;; longs per index.  (Could keep count in same bits but that doesn't save much.)

;;; one byte is enough for count, 7 bytes for flags -- but the calcs not worth it?

;;; Could do two-bits per index.  00 unk, 10 good, 11 bad




;;; baxter true
(def b20 [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19])

;;; non-baxter so has to test more
(def nb20 [0 10 2 3 4 5 6 7 8 12 9 12 13 14 1 16 17 18 19])

(defn vp-test []
  (let [vp? (vp-fn "3-14-2")]
    (println "(vp? [5 3 1 4 2])")
    (assert (vp? [5 3 1 4 2]))
    (println "(vp? [3 1 4 6 2])")
    (assert (vp? [3 1 4 6 2]))
    (assert (not (vp? b20)))
    (assert (vp? nb20))
    true))


;;;; NEEDS MORE TESTING

;;; baxter the hard way
(def bxx? (complement (some-fn (vincular-pattern-fn "3-14-2") (vincular-pattern-fn "2-41-3"))))

(def bxx2? (complement (some-fn (vp-fn "3-14-2") (vp-fn "2-41-3"))))

;;; Need a more complex pattern to test

(let [p3142? (vincular-pattern-fn "3-14-2")
      p2413? (vincular-pattern-fn "2-41-3")]
  (defn cbax? [v]
    (not (or (p3142? v) (p2413? v)))))



;;; startv is the min indices [A B C]
;;; first check apv for each
;;; if A fails inc A and try again until good A
;;;   then B similarly, plus check vinc-AB until good B
;;;   then C similarly, plus check full vinc (ABC) until good C --> success
;;;   or out of C, then backtrack to inc B again
;;;   same backtrack to inc C again

;;; backtrack = inc previous P and reset Q to (init-Q P) based on widths

;;; might be simpler to init with [0] and add new index as you go (+ (width q) p)



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


        


#_
(require '[clojure.math.combinatorics :as mc])
            


;;; ----------------------------------------------------------------------
;;; older junk


(defn canonical-perm [v]
  (let [remap (zipmap (sort v) (range 1 (inc (count v))))]
    (mapv remap v)))


(defn WAScontains231? [v]
  ;; v is permuation vector of 1..N when N is count
  (some #(= (canonical-perm %) [2 3 1]) (mc/combinations v 3)))



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


(defn vincular-pattern-fn-orig [pat]
  ;; (assert (pos? (count pat)) "Empty pat")
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        apv (mapv apat-fn pat)
        vpv (into [(constantly true)] (map vinc-fn) (rest (rest (reductions conj [] pat))))]
    ;;; seem faster to (map #(vinc-record (subvec ppp 0 %)) (range 2 (inc cnt)))

    (fn ([] pat)
      ([v]
       (let [len (count v)
             maxv (mapv #(- len %) endsp)]
         (loop [ijk [0]]
           (let [ith (dec (count ijk))
                 i (peek ijk)]
             (cond (nil? i) false
                   (> i (maxv ith))
                       (let [ijk2 (pop ijk)]
                         (recur (when-not (zero? (count ijk2))
                                  (conj (pop ijk2) (inc (peek ijk2))))))
                   (not ((apv ith) v i))   (recur (conj (pop ijk) (inc i)))
                   (not ((vpv ith) v ijk))   (recur (conj (pop ijk) (inc i)))
                   (= ith (dec cnt))   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijk (+ (peek ijk) (cntv ith))))))))))))



;;; old way had convenience no-arg to recall adjpat (for debugging)
(defn apat-fn1 [adjpat]
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

;;; thought of memoizing #(ap v %) rather than ap
;;; use int-map for cache
;;; make a transducer memo with storage option

;;; tried memoize with apv fn but still slow -- but that was checking two args, new idea is
;;; to specialize with closed v

(defn int-memoize
  "Specialized version of `memoize` that optimizes for `f` taking a single integer arg"
  [f]
  (let [mem (atom (im/int-map))]
    (fn [arg]
      (if-let [e (find @mem arg)]
        (val e)
        (let [ret (f arg)]
          (swap! mem assoc arg ret)
          ret)))))

;;; experiment with int-memoize but not faster
(defn vp-fn3 [pat]
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        apv (mapv apat-fn pat)
        vpv (into [(constantly true)]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]
    (assert (pos? cnt) "Empty pat")

    (fn ([] pat)
      ([v]
       (let [len (count v)
             memapv (mapv (fn [a] (int-memoize #(a v %))) apv)
             maxv (mapv #(- len %) endsp)]
         (loop [ijk [0]]
           (let [ith (dec (count ijk))
                 i (peek ijk)]
             (cond (nil? i) false
                   (> i (maxv ith))
                       (let [ijk2 (pop ijk)]
                         (recur (when-not (zero? (count ijk2))
                                  (conj (pop ijk2) (inc (peek ijk2))))))
                   (not ((memapv ith) i))   (recur (conj (pop ijk) (inc i)))
                   (not ((vpv ith) v ijk))   (recur (conj (pop ijk) (inc i)))
                   (= ith (dec cnt))   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijk (+ (peek ijk) (cntv ith))))))))))))


;;; RECONSIDERING -- better to keep stack of index ranges.  Use old logic but grab original
;;; apv range and bump with old logic

;;; WORKS but slow for test-baxter
(defn vp-fn1 [pat]
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        apv (mapv apat-fn pat)
        vpv (into [(constantly true)]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]
    (assert (pos? cnt) "Empty pat")

    (fn ([] pat)
      ([v]
       (let [maxv (mapv #(- (inc (count v)) %) endsp)
             ;;_ (println "maxv" maxv)
             rngv (mapv (fn [x] (filter #((apv x) v %) (range (nth cntv (dec x) 0) (maxv x))))
                        (range cnt))]
         ;; (println "rngv" rngv)
         (loop [ijkv [(rngv 0)]]
           ;; (println "ijkv" ijkv)
           (let [ith (dec (count ijkv))
                 ijk (mapv first ijkv)
                 i (peek ijk)]
             ;; (println "  ijk" ijk)
             (cond (neg? ith) false

                   (nil? i) (let [ijkv2 (pop ijkv)]
                              (recur (when-not (zero? (count ijkv2))
                                       (conj (pop ijkv2)
                                             (rest (peek ijkv2))))))

                   (not ((vpv ith) v ijk))   (recur (conj (pop ijkv) (rest (ijkv ith))))
                   (= (inc ith) cnt)   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijkv (drop-while #(< % (+ (peek ijk) (cntv ith)))
                                                        (rngv (inc ith)))))))))))))

;;; slower but I like the idea
(defn vp-fn-slow-filt [pat]
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        apv (mapv apat-fn pat)
        vpv (into [(constantly true)]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]
    (assert (pos? cnt) "Empty pat")

    (fn [v]
       (let [width (inc (count v))
             maxv (mapv #(- width %) endsp)
             ;;_ (println "maxv" maxv)
             rngv (mapv (fn [af start end] (filter #(af v %) (range start end)))
                        apv
                        (into [0] cntv)
                        maxv)]

         ;; (println "rngv" rngv)
         (loop [ijkv [(rngv 0)]]
           ;; (println "ijkv" ijkv)
           (let [ith (dec (count ijkv))
                 ijk (mapv first ijkv)
                 i (peek ijk)]
             ;; (println "  ijk" ijk)
             (cond (neg? ith) false

                   (nil? i) (let [ijkv2 (pop ijkv)]
                              (recur (when-not (zero? (count ijkv2))
                                       (conj (pop ijkv2)
                                             (rest (peek ijkv2))))))

                   (not ((vpv ith) v ijk))   (recur (conj (pop ijkv) (rest (ijkv ith))))
                   (= (inc ith) cnt)   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijkv (drop-while #(< % (+ (peek ijk) (cntv ith)))
                                                        (rngv (inc ith))))))))))))


;;; only do the filter on demand, still not so good but a bit better for some cases
;;; I wonder if filter chunck size is getting us?  Or just too much collection manipualtion?

;;; Probably need to analyse the pattern and do the longs adj pat first!

(defn vp-fn4 [pat]
  (let [pat (vincular-pattern pat)
        cnt (count pat)
        cntv (mapv count pat)
        endsp (reductions - (reduce + 0 cntv) (pop cntv))
        apv (mapv apat-fn pat)
        rngx (fn [v x st end]
               ;; st is actually the current previous index, add width to get start
               (filter #((apv x) v %)
                       (range (+ st (nth cntv (dec x) 0)) end)))
        vpv (into [(constantly true)]
                  (comp (map #(subvec pat 0 %)) (map vinc-fn))
                  (range 2 (inc cnt)))]
    (assert (pos? cnt) "Empty pat")

    (fn [v]
       (let [width (inc (count v))
             maxv (mapv #(- width %) endsp)
             ;;_ (println "maxv" maxv)
             rngv (mapv (fn [af start end] (filter #(af v %) (range start end)))
                        apv
                        (into [0] cntv)
                        maxv)]

         ;; (println "rngv" rngv)
         (loop [ijkv [(rngx v 0 0 (maxv 0))]]
           ;; (println "ijkv" ijkv)
           (let [ith (dec (count ijkv))
                 ijk (mapv first ijkv)
                 i (peek ijk)]
             ;; (println "  ijk" ijk)
             (cond (neg? ith) false

                   (nil? i) (let [ijkv2 (pop ijkv)]
                              (recur (when-not (zero? (count ijkv2))
                                       (conj (pop ijkv2)
                                             (rest (peek ijkv2))))))

                   (not ((vpv ith) v ijk))   (recur (conj (pop ijkv) (rest (ijkv ith))))
                   (= (inc ith) cnt)   ijk ;; success
                   ;; good so far, add another index
                   :else  (recur (conj ijkv (rngx v (inc ith)
                                                  ;;(+ (peek ijk) (cntv ith))
                                                  (peek ijk)
                                                  (maxv (inc ith))))))))))))
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

    
