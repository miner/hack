(ns miner.stack-sortable
  (:require [clojure.math.combinatorics :as mc]
            [clojure.string :as str]))

;;; https://en.wikipedia.org/wiki/Stack-sortable_permutation

;;; Note: this works for many collections, but not if coll contains the pattern 231
(defn naive-stack-sort [coll]
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

;;; reord for adjpat, return adj test fn which checks partial order for that block or
;;; returns good block or nil.
(defn subv-fn [adjpat]
  (let [cnt (count adjpat)
        reord (mapv peek (sort (map-indexed (fn [i p] (vector p i)) adjpat)))]
    (assert (pos? cnt) "Empty adjpat")
    (fn [v i]
      (transduce (map #(nth v (+ i %)))
                 (fn ([r x] (if (< r x) x (reduced nil)))
                   ([r] (when r (subvec v i (+ i cnt)))))
                 -1
                 reord))))




;;; returns vector of adjacency vectors [[2 3] [1 4]]
(defn vincular-pattern [pat]
  (if (vector? pat)
    pat
    (mapv (fn [a] (mapv #(- (long %) (long \0)) a)) (str/split (str pat) #"-"))))

(declare vincular-pattern-fn)

;;; FIXME, BUGGY, NOT IMPLEMENTED
;;; need to generate subcolls in proper size according to adjacency vectors
;;; then remap them in proper order
#_
(defn vincular-pattern-fn [pat]
  (let [p (vincular-pattern pat)
        adjcnt (count p)
        pm (zipmap (map dec p) (range))]
    (println "pattern-fn " pm)
    (fn [xv]
      (and (>= (count xv) cnt)
           (some (fn [q] (transduce (map #(nth q (get pm %)))
                                    (fn ([r x] (if (< r x) x (reduced -1))) ([r] (pos? r)))
                                    -1
                                    (range cnt)))
                 (mc/combinations xv cnt))))))

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
