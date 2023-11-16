(ns miner.stack-sortable
  (:require [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;;; https://en.wikipedia.org/wiki/Stack-sortable_permutation

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



;;; Note: this works for many collections, but not if coll contains the pattern 231
(defn bogus-stack-sort [coll]
  (loop [stack [] xs (seq coll) res []]
    (if (empty? xs)
      (into res (rseq stack))
      (let [p (peek stack)]
        (if (and p (> (first xs) p))
          (recur (pop stack) xs (conj res p))
          (recur (conj stack (first xs)) (rest xs) res))))))


(defn orig-bogus-stack-sort [coll]
  (loop [stack [] xs (seq coll) res []]
    (if (empty? xs)
      (into res (rseq stack))
      (let [p (peek stack)
            x (first xs)]
        (if (nil? p)
          (recur (conj stack x) (rest xs) res)
          (if (> x p)
            (recur (pop stack) xs (conj res p))
            (recur (conj stack x) (rest xs) res)))))))






;;; can't use reduce because sometimes you need to reconsider the x (when x>p)


(defn test-ss [ss]
  (remove #(= (ss %) (sort %)) (mc/permutations (range 1 4))))

;;; p is a permuation of 1..N
(defn contains231? [p]
  (some (fn [[a b c]] (< c a b)) (mc/combinations p 3)))






;;; converts 231 or "231" to [2 3 1]
(defn one-line-pattern [pat]
  (if (vector? pat)
    pat
    (mapv #(- (long %) (long \0)) (str pat))))



;;; This version handles case of pat possibly being within larger pattern (not exact size match)
;;; only handle "one-line" notation, not vincular
(defn one-line-pattern-fn [pat]
  (let [p (one-line-pattern pat)
        cnt (count p)
        ;; _ (assert (and (< cnt 10) (every? #(<= 1 % 9) p))
        ;;     (str "pattern-fn expects 1..9+, failed: " pat))
        pm (zipmap (map dec p) (range))]
    (println "pattern-fn " pm)
    (fn [xv]
      (and (>= (count xv) cnt)
           (some (fn [q] (transduce (map #(nth q (get pm %)))
                                    (fn ([r x] (if (< r x) x (reduced -1))) ([r] (pos? r)))
                                    -1
                                    (range cnt)))
                 (mc/combinations xv cnt))))))


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



;; but you actually need to leave enough room between them, not just <
;; 6 1 3
;; 2 0 1

;;; better idea to check the gaps
;;;  reord [[index width] ...]
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


;; simpler reord [[index width] ...]
(defn apat4-fn [adjpat]
  (let [cnt (count adjpat)
        sortas (sort adjpat)
        reord (map vector
                   (map first (sort-by peek (map-indexed vector adjpat)))
                   (into [0] (map - (rest sortas) sortas)))]
    (println "apat-fn" reord)
    (assert (pos? cnt) "Empty adjpat")
    (if (= cnt 1)
      (fn ([] adjpat) ([v i] i))
      (fn
        ([] adjpat)
        ([v i]
         (transduce (map (fn [[j w]] [(nth v (+ i j)) w]))
                    (fn ([r [x w]]
                         (println "af" r x w)
                         (if (<= (+ r w) x) x (reduced nil)))
                      ([r] (when r i)))
                    -1
                    reord))))))



;;; better reord 
(defn apat3-fn [adjpat]
  (let [cnt (count adjpat)
        reord (pop (reduce (fn [r ia]
                             (let [a (peek ia)]
                               (if-let [a1 (peek r)]
                                 (conj (pop r) (conj (pop ia) (- a a1)) a)
                                 (conj r (conj (pop ia) 0) a))))
                      []
                      (sort-by peek (map-indexed vector adjpat))))]
    (println "apat-fn" reord)
    (assert (pos? cnt) "Empty adjpat")
    (if (= cnt 1)
      (fn ([] adjpat) ([v i] i))
      (fn
        ([] adjpat)
        ([v i]
         (transduce (map (fn [[j w]] [(nth v (+ i j)) w]))
                    (fn ([r [x w]]
                         (println "af" r x w)
                         (if (<= (+ r w) x) x (reduced nil)))
                      ([r] (when r i)))
                    -1
                    reord))))))






(defn apat2-fn [adjpat]
  (let [cnt (count adjpat)
        reord (reduce (fn [r ia]
                        (if-let [a1 (nth (peek r) 1 nil)]
                          (conj r (conj ia (- (nth ia 1) a1)))
                          (conj r (conj ia 0))))
                      []
                      (sort-by peek (map-indexed vector adjpat)))]
    (println "apat-fn" reord)
    (assert (pos? cnt) "Empty adjpat")
    (if (= cnt 1)
      (fn ([] adjpat)
        ([v i] i))
      (fn
        ([] adjpat)
        ([v i]
         (transduce (map (fn [[j _ w]] [(nth v (+ i j)) w]))
                    (fn ([r [x w]]
                         (println "af" r x w)
                         (if (<= (+ r w) x) x (reduced nil)))
                      ([r] (when r i)))
                    -1
                    reord))))))



(defn apat1-fn [adjpat]
  (let [cnt (count adjpat)
        reord (mapv #(vector (nth % 0) (nth % 2))
                    (reduce (fn [r ia]
                              (if-let [a1 (nth (peek r) 1 nil)]
                                (conj r (conj ia (- (nth ia 1) a1)))
                                (conj r (conj ia 0))))
                      []
                      (sort-by peek (map-indexed vector adjpat))))]
    (println "apat-fn" reord)
    (assert (pos? cnt) "Empty adjpat")
    (fn
      ([] adjpat)
      ([v i]
      (transduce (map (fn [[j w]] [(nth v (+ i j)) w]))
                 (fn ([r [x w]]
                      (println "af" r x w)
                      (if (<= (+ r w) x) x (reduced nil)))
                   ([r] (when r i)))
                 -1
                 reord)))))


;;; returns vector of adjacency vectors [[2 3] [1 4]]
(defn vincular-pattern [pat]
  (if (vector? pat)
    pat
    (mapv (fn [a] (mapv #(- (long %) (long \0)) a)) (str/split (str pat) #"-"))))


;;; reord is flat index remap
;;; [[1  0]  [2  7  3  5]  [4  6]]
;;; [1  0    2  7  3  5     4  6]
;;;  3  4   10 11 12 13     20 21

;;; FIXME:  how do you wire a reord perm function?  Can you synthesize a fn that reords by args?
;;; Without having to remap at runtime.

;;; not exactly the right thing
(defn ijks [xv p apfs] nil)

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
(defn vincular-pattern-fn [pat]
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
(defn sample1 []
  (let [pv [[4 3] [2] [1 6 5]]
        cntv (mapv count pv)
        spacev (vec (reductions - (reduce + 0 cntv) (pop cntv)))
        v (vec (range 10 21))
        len (count v)
        maxv (mapv #(- (inc len) %) spacev)]
    ;; (println "pv" pv ", v" v ", max xyz" xmax ymax zmax)
    (for [x (range 0 (maxv 0))
          y (range (+ x (count (first pv))) (maxv 1))
          z (range (+ y (count (second pv))) (maxv 2))]
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