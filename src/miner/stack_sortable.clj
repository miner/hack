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


;;; returns vector of adjacency vectors [[2 3] [1 4]]
(defn vincular-pattern [pat]
  (if (vector? pat)
    pat
    (mapv (fn [a] (mapv #(- (long %) (long \0)) a)) (str/split (str pat) #"-"))))

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



(defn canonical-perm [v]
  (let [remap (zipmap (sort v) (range 1 (inc (count v))))]
    (mapv remap v)))


(defn WAScontains231? [v]
  ;; v is permuation vector of 1..N when N is count
  (some #(= (canonical-perm %) [2 3 1]) (mc/combinations v 3)))


(defn cbax? [v]
  (let [p3142? (pattern-fn "3-14-2")
        p2413? (pattern-fn "2-41-3")]
    (not (or (p3142? v) (p2413? v)))))
