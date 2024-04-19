(ns miner.permutations
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))


;;; Huge paper on math of permutation patterns and generation.  Most of it is beyond my
;;; comprehension but it looks useful.
;;;
;;; Combinatorial generation via permutation languages.
;;; by  Hartung, et.al.
;;; https://www.researchgate.net/publication/333815377_Combinatorial_generation_via_permutation_languages_I_Fundamentals

;;; another good paper (oldish) on generating permutations
;;; https://www.princeton.edu/~rblee/ELE572Papers/p137-sedgewick.pdf

;; SEM: NB, use math.combinatorics not this!
;; I was experimenting with writing my own permutations.

;;; Correction:  look at this page on "Permuations Pattern"
;;; https://en.wikipedia.org/wiki/Permutation_pattern

;;; THIS NEEDS A REWRITE to distinguish between a permuation and a permutation pattern.  I'm
;;; too tight on a permutation and too lose on pattern, where should be "permutation pattern".
;;; This terminology is used with combinatorial mathementics and computer science
;;; (especially starting with Knuth).

;;; I might make some of my own definitions of terms.  It probably makes sense to use the
;;; "one-line notation" and "dash notation".

;;; In Clojure, a permutation of size N is some unique ordering of the ints in (range N).
;;; It's zero-based, whereas most of the mathematical literature is 1-based for sequences
;;; and patterns.  [You need to find a reference for this.]

;;; For most integer patterns, we typically only care about the relative positioning of the
;;; elements.  For example, we might care that the highest value occurs before the lowest in
;;; the pattern sequence, regardless of the exact values.  For these kind of questions, we
;;; can reduce the problem to consideration of the indicies of the values in sorted order.




;;; This took me a while to understand in the Wikipedia permutation pattern discussion.
;;; Also, the math literature generally is 1-based for the lowest index, whereas Clojure is
;;; naturally zero-based.  My code here is zero-based.

;;; "Over-excited" is my term for a pattern that has elements greater than or equal to the
;;; count of the number of distinct elements in the pattern.  The math literature seems to
;;; implicitly disallow these expressions as patterns.  Probaly need to find a more formal
;;; definition to confirm this statement.

;;; remap over-excited pattern into base indices (zero-based).
(defn canonical-pattern [pat]
  (let [pat-map (zipmap (sort (distinct pat)) (range))]
    (mapv pat-map pat)))

;;; For example,
;;; (canonical-pattern [41 2 4 1 4 6])
;;; => [4 1 2 0 2 3]
;;; because value 1 is the lowest int (zero-nth), and 41 is the highest ("4-nth" or fifth).



;;; remap a (random) into vector into a Clojure permuation.  Note duplicates are dropped.
(defn vector->permutation [v]
  (let [dist (distinct v)
        sorted (sort dist)
        order-map (zipmap sorted (range))]
    (mapv order-map dist)))


;; http://stackoverflow.com/questions/2087693/how-can-i-get-all-possible-permutations-of-a-list-with-common-lisp

#_ (defun all-permutations (lst &optional (remain lst))
  (cond ((null remain) nil)
        ((null (rest lst)) (list lst))
        (t (append
            (mapcar (lambda (l) (cons (first lst) l))
                    (all-permutations (rest lst)))
            (all-permutations (append (rest lst) (list (first lst))) (rest remain))))))

;; translating to Clojure
;; but recursive, non-tail, and slow
(defn all-permutations-slow [coll]
  (letfn [(all-perm
            ([coll] (all-perm coll coll))
            ([coll remain]
             (cond (nil? (seq remain)) nil
                   (nil? (next coll)) (list coll)
                   :else (concat
                          (map (fn [l] (cons (first coll) l))
                               (all-perm (rest coll)))
                          (all-perm (concat (rest coll) (list (first coll))) (rest remain))))))]
    (all-perm coll)))


;; http://stackoverflow.com/questions/13689172/better-permutations-generating-algorithm
;; (defun i-permute-quickperm (array)
;;   (let* ((len (length array))
;;          (markers (make-vector len 0))
;;          (i 1) j)
;;     (while (< i len)
;;       (if (< (aref markers i) i)
;;           (progn
;;             (setq j (if (oddp i) (aref markers i) 0))
;;             (i-swap array j i)
;;             (message "array: %s" array)
;;             (aset markers i (1+ (aref markers i)))
;;             (setq i 1))
;;         (aset markers i 0)
;;         (incf i)))))

;; rough translation, not lazy, but we can fix that
(defn rough-permutations [coll]
  (let [v (vec coll)
        len (count v)
        markers (long-array len)]
    (loop [v v vs (list v) i 1]
      (if (< i len)
        (if (< (aget markers i) i)
          (let [j (if (odd? i) (aget markers i) 0)
                vj (nth v j)
                v (assoc v j (nth v i) i vj)]
            (aset markers i (inc (aget markers i)))
            (recur v (conj vs v) 1))
          (do
            (aset markers i 0)
            (recur v vs (inc i))))
        vs))))


;; See also Heap's Algorithm
;; http://en.wikipedia.org/wiki/Heap%27s_algorithm
;;
;; Note: cons is faster than conj in inner loop.
;; aset is faster than aset-int (why?)
(defn heaps-permutations [coll]
  (let [v (vec coll)
        len (count v)
        markers (int-array len)]
     (loop [vs (list v) i 1]
       (if (< i len)
         (if (< (aget markers i) i)
           (let [v (first vs)
                 j (if (odd? i) (aget markers i) 0)
                 v (assoc v j (v i) i (v j))]
             (aset markers i (inc (aget markers i)))
             (recur (cons v vs) 1))
           (do
             (aset markers i 0)
             (recur vs (inc i))))
         vs))))

(defn lazy-permutations [coll]
  (lazy-seq
   (let [v (vec coll)
         len (count v)
         markers (int-array len)
         stepfn (fn step [i v]
                  (lazy-seq
                   (loop [i i v v]
                    (when (< i len)
                      (if (< (aget markers i) i)
                        (let [j (if (odd? i) (aget markers i) 0)
                              v (assoc v j (v i) i (v j))]
                          (aset markers i (inc (aget markers i)))
                          (cons v (step 1 v)))
                        (do
                          (aset markers i 0)
                          (recur (inc i) v)))))))]
     (cons v (stepfn 1 v)))))

;; loop/recur is slightly faster than recursive step at end of stepfn
(defn OK-lazy-permutations [coll]
  (lazy-seq
   (let [v (vec coll)
         len (count v)
         markers (int-array len)
         stepfn (fn step [i v]
                  (lazy-seq
                   (loop [i i v v]
                    (when (< i len)
                      (if (< (aget markers i) i)
                        (let [j (if (odd? i) (aget markers i) 0)
                              v (assoc v j (nth v i) i (nth v j))]
                          (aset markers i (inc (aget markers i)))
                          (cons v (step 1 v)))
                        (do
                          (aset markers i 0)
                          (recur (inc i) v)))))))]
     (cons v (stepfn 1 v)))))



;; ugly but it works and is lazy
(defn UGLY-lazy-permutations [coll]
  (let [v (vec coll)
        len (count v)
        markers (int-array len)
        stepper (fn step [i v]
                  (lazy-seq 
                   (when (< i len)
                     (if (< (aget markers i) i)
                       (let [j (if (odd? i) (aget markers i) 0)
                             v (assoc v j (nth v i) i (nth v j))]
                         (aset markers i (inc (aget markers i)))
                         (cons v (step 1 v)))
                       (do
                         (aset markers i 0)
                         (step (inc i) v))))))]
    (lazy-seq (cons v (stepper 1 v)))))

;; wrong idea was to just wrap a lazy-seq around quick-perm loop
;; looked OK at first for take, but only because take is lazy itself
;; as soon as you needed anything, it forced the full loop to run -- not really lazy


;; SEM: why is aset faster than aset-long?  Extra checking?



;;;;;;;;;; borrowed from halfbaked
(def demangle-replacements
  (array-map "_QMARK_" "?"
             "_BANG_" "!"
             "_STAR_" "*"
             "_GT_" ">"
             "_EQ_" "="
             "_PLUS_" "+"
             "_LT_" "<"
             "_SLASH_" "/"
             "_AMPERSAND_" "&"
             "_TILDE_" "~"
             ;; keep underbar last
             "_" "-"))

;; a faster but ugly version is in demangle.clj
(defn ^String demangle
  "Demangle a clojure identifier name"
  [^String s]
  (reduce-kv str/replace s demangle-replacements))

(defn compiled-fn-name
  "returns the simple name (a string) for the given function f as determined by the compiler"
  [f]
  (let [f (if (var? f) (var-get f) f)
        compiled-name (when (fn? f) (str f))
        fname (second (first (re-seq #"[$](.*)@" compiled-name)))]
    (if fname
      (demangle fname)
      compiled-name)))


;; SEM should use criterium to benchmarking

(defn test-perm
  ([] (test-perm lazy-permutations))
  ([pfn] (test-perm 9 pfn))
  ([n pfn]
  (let [v (vec (range n))
        trials 3]
    (when (= (set (combo/permutations v)) (set (pfn v)))
      (println "combo count")
      (dotimes [_ trials]
        (time (count (combo/permutations v))))
      (println (compiled-fn-name pfn) "count")
      (dotimes [_ trials]
        (time (count (pfn v))))
      (println "combo add take 3")
      (dotimes [_ trials]
        (time (reduce + (apply concat (take 3 (combo/permutations v))))))
      (println (compiled-fn-name pfn) "add take 3")
      (dotimes [_ trials]
        (time (reduce + (apply concat (take 3 (pfn v))))))))))
      


;;; 12/02/18  17:34 by miner -- want a simpler version that just does N ints
;;; non-lazy is fine

(defn cperm [n]
  ;; lazy and lexicographic
  (combo/permutations (range n)))

;; somewhat faster than (c/permuations (range n)) -- but eager, not lazy
;; also, not lexicographic so it's not as nice to use for some purposes.
(defn range-permutations
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

;; experimenting
(defn rperm
  "Returns an eager sequence of vectors representing the permutations of the half-open
  range [0, N).  Not lexicographic."
  [n]
  (reduce (fn [vs cnt]
            (reduce (fn [acc vvv]
                      (reduce-kv (fn [r i x] (conj r (assoc vvv i cnt cnt x)))
                                 (conj acc (conj vvv cnt))
                                 vvv))
                    ()
                    vs))
          (list [])
          (range n)))


;; fastest permutations algorith: Heap's Algorithm
;; https://en.wikipedia.org/wiki/Heap%27s_algorithm

;; procedure generate(n : integer, A : array of any):
;;     if n = 1 then
;;           output(A)
;;     else
;;         for i := 0; i < n - 1; i += 1 do
;;             generate(n - 1, A)
;;             if n is even then
;;                 swap(A[i], A[n-1])
;;             else
;;                 swap(A[0], A[n-1])
;;             end if
;;         end for
;;         generate(n - 1, A)
;;     end if


;; not so fast
(defn rheaps-SAVE
  ([n] (rheaps-SAVE n (vec (range n)) () ))
  ([n a res]
   (if (= n 1)
     (conj res a)
     (let [n1 (dec n)]
       (loop [i 0 a a res res]
         (if (< i n1)
           (let [res1 (rheaps-SAVE n1 a res)
                 a (first res1)
                 a1 (if (even? n)
                      (assoc a i (a n1) n1 (a i))
                      (assoc a 0 (a n1) n1 (a 0)))]
             (recur (inc i) a1 res1))
           (rheaps-SAVE n1 a res)))))))

(defn rheaps
  ([n] (rheaps n (vec (range n)) () ))
  ([n a res]
   (if (= n 1)
     (conj res a)
     (let [n1 (dec n)]
       (loop [i 0 a a res res]
         (if (< i n1)
           (let [res1 (rheaps n1 a res)
                 a (first res1)
                 a1 (if (even? n)
                      (assoc a i (a n1) n1 (a i))
                      (assoc a 0 (a n1) n1 (a 0)))]
             (recur (inc i) a1 res1))
           (rheaps n1 a res)))))))





;; procedure generate(n : integer, A : array of any):
;;     c : array of int
;; 
;;     for i := 0; i < n; i += 1 do
;;         c[i] := 0
;;     end for
;; 
;;     output(A)
;;     
;;     i := 0;
;;     while i < n do
;;         if  c[i] < i then
;;             if i is even then
;;                 swap(A[0], A[i])
;;             else
;;                 swap(A[c[i]], A[i])
;;             end if
;;             output(A)
;;             c[i] += 1
;;             i := 0
;;         else
;;             c[i] := 0
;;             i += 1
;;         end if
;; end while
;; 

(defn heaps-slow [n]
  (loop [i 0, a (vec (range n)), c (vec (repeat n 0)), res (list a)]
    (if (= i n)
      res
      (if (< (c i) i)
        (let [a2 (if (even? i)
                   (assoc a 0 (a i) i (a 0))
                   (assoc a (c i) (a i) i (a (c i))))]
          (recur 0 a2 (update c i inc) (conj res a2)))
        (recur (inc i) a (assoc c i 0) res)))))


(defn heaps2-slow [n]
  (loop [i 0, c (vec (repeat n 0)), res (list (vec (range n)))]
    (if (= i n)
      res
      (if (< (c i) i)
        (let [a (first res)
              a2 (if (even? i)
                   (assoc a 0 (a i) i (a 0))
                   (assoc a (c i) (a i) i (a (c i))))]
          (recur 0 (update c i inc) (conj res a2)))
        (recur (inc i) (assoc c i 0) res)))))





;; significantly faster to use mutable int-array for bookkeeping rather than vector.
;; ends up being a little faster than my range-permuations
(defn heaps-permutations [n]
  (let [c (int-array n 0)]
    (loop [i 0, a (vec (range n)), res (list a)]
      (if (= i n)
        res
        (if (< (aget c i) i)
          (let [a2 (if (even? i)
                     (assoc a 0 (a i) i (a 0))
                     (assoc a (aget c i) (a i) i (a (aget c i))))]
            (aset c i (inc (aget c i)))
            (recur 0 a2 (conj res a2)))
          (do
            (aset c i 0)
            (recur (inc i) a res)))))))




;;; from Sedgewick paper
;;; Algorithm 8 (Ord-Smith)
;;
;; i:=N; loop c[i]:=1; while i>2 : i:= i-1 repeat;
;; process;
;; loop: if c[i] < i then P[i]:= P[c[i]], reverse(i-1) ,
;;                 c[i]:=c[i]+ 1; i:=2;
;;                 process;
;;       else c[i]:=1; i:= i+1
;; endif;
;; while i<=N repeat;

(defn rev [pv i]
  (into (into [] (rseq (subvec pv 0 i)))
        (subvec pv (subvec pv i))))

;;; UNFINISHED
(defn ord-smith-permutations [n]
  (let [c (vec (repeat n 1))]
    (reduce (fn [vs i]
              (let [p (peek vs)]
              (if (< (c i) i)
                (-> p
                    (assoc i (p (c i)))
                    (rev i))

              )
            [(vec (range n))]
            (range n))))))
    
    




;;; https://codereview.stackexchange.com/questions/195727/mutative-heaps-algorithm-permutations-generator-in-clojure

;;; refactored by SEM for easier changes.  The recursive swaps doesn't look efficient to me.
;;; SAVE THIS VERSION
(defn swaps1 [n]
  ;; generate pair of indices
  (if (= n 1)
    ()
    (let [base (swaps1 (dec n))
          extras (if (odd? n) (repeat (dec n) 0) (range (dec n)))]
      (concat
       base
       (mapcat (fn [x] (cons [x (dec n)] base)) extras)))))


;;; Original code from stack-exchange, just refactored for easier comparison.  I didn't like
;;; the recursive swaps.  I worked out a better way below in my crperm2, which uses tail
;;; recursion and transducers for better performance.

(defn crperm1 [n]
  (letfn [(swaps [n]
            ;; generate pair of indices
            (if (= n 1)
              ()
              (let [base (swaps (dec n))
                    extras (if (odd? n) (repeat (dec n) 0) (range (dec n)))]
                (concat
                 base
                 (mapcat (fn [x] (cons [x (dec n)] base)) extras)))))          
          
          (perms [v]
            ;; v is the original elements in a vector
            (reductions
             (fn [a [i j]] (assoc a i (a j) j (a i)))
             v
             (swaps (count v))))]
    (perms (vec (range n)))))


#_ (quick-bench (count (crperm1 9)))
;;;=> 362880

#_ (quick-bench (nth (crperm3 9) 150))
;;; Execution time mean : 3.455148 ms
;;; good test of laziness




;;; Note: this is described as an implementation of Heap's algorithm.  I have another
;;; version above called heaps-permutations that is faster.  It looks quite different as the
;;; swaps are controlled by counters rather than realized as a sequence.  My version
;;; crperm2 is probably easier to read.  But there's still some mystery associated with how
;;; it works.

;;; refactoring and consolidating for a cleaner, faster implementation, but maybe not so
;;; easy to understand.  BTW, it is brittle with respect to the ordering of the swaps [i j]
;;; Also, not lazy.  That might be an issue for some applications.  (Original is lazy with
;;; reductions.  And swaps are fairly lazy.  Anway, I have a much faster eager version and a
;;; somewhat faster lazy version in crperm3 below.)


(defn crperm2 [cnt]
  (reductions
   (fn [a [i j]] (assoc a i (a j) j (a i)))
   (vec (range cnt))
   (reduce (fn [base n]
             (if (odd? n)
               (into base (mapcat #(cons [% n] base)) (range n))
               (into base cat (repeat n (cons [0 n] base)))))
           []
           (range 1 cnt))))


;;; fastest swaps but eager, not lazy
(defn eswaps [cnt]
  (reduce (fn [base n]
            (if (odd? n)
              (into base (mapcat #(cons [% n] base)) (range n))
              (into base cat (repeat n (cons [0 n] base)))))
          []
          (range 1 cnt)))


;;; maybe useful later?  the count of swaps is n!-1
;;; cnt (if (<= n 1) 0 (reduce * -1 (range 1 (inc n))))




;;; Lazy fast, but slow compared to eager eswaps
(defn lswaps [cnt]
  (let [step (fn step [n base]
               (when (< n cnt)
                 (let [ext (if (odd? n)
                             (mapcat #(cons [% n] base) (range n))
                             (sequence cat (repeat n (cons [0 n] base))))]
                   (concat ext (lazy-seq (step (inc n) (concat base ext)))))))]
    (lazy-seq (step 1 nil))))

;;; lazy and fairly fast, about 2x crperm2
(defn crperm3 [cnt]
  (let [step (fn step [n base]
               (when (< n cnt)
                 (let [ext (if (odd? n)
                             (mapcat #(cons [% n] base) (range n))
                             (sequence cat (repeat n (cons [0 n] base))))]
                   (concat ext (lazy-seq (step (inc n) (concat base ext)))))))]
    (reductions
     (fn [a [i j]] (assoc a i (a j) j (a i)))
     (vec (range cnt))
     (lazy-seq (step 1 nil)))))




