(ns miner.index)

;; REALLY OLD, not reliable

;; SEM index-of should have coll first?

;; requires vector arg, fastest
(defn index-of [val vector]
  (let [limit (count vector)]
    (loop [i 0]
      (cond
       (>= i limit) nil
       (= val (vector i)) i
       :else (recur (inc i))))))
      
;; more general
(defn index2 [val coll]
  (loop [i 0 coll (seq coll)]
    (cond
     (empty? coll) nil
     (= (first coll) val) i
     :else (recur (inc i) (rest coll)))))


;; slower but short
(defn index3 [val coll]
  (first (keep-indexed #(when (= val %2) %1) coll)))

;; derived from cgrand on the mailing list
(defn index4 [val coll]
  (let [i (.indexOf ^java.util.List coll val)]
    (and (not (neg? i)) i)))

;; cgrand on the mailing list
(defn sv [vector value] 
  (let [i (.indexOf ^java.util.List vector value)]
    (if (neg? i) [] (subvec vector i))))


(def v100 (vec (range 10 110)))


;;; 03/09/17  12:34 by miner -- new idea:  call it `whence` with a predicate

;;; Note: it's wrong to expect reduce-kv to work with a seq.  It's specialized for a map and
;;; a vector, not a general collection or seq.

(defn whence [pred coll]
  (loop [i 0 coll (seq coll)]
    (cond (not coll) nil
          (pred (first coll)) i
          :else (recur (inc i) (rest coll)))))

(defn whence-kv [pred2 kvs]
  (reduce-kv (fn [_ k v] (when (pred2 k v) (reduced k))) nil kvs))



;; https://whynotsoftware.github.io/wnaf-list-index-of/
(defn list-index-of
  [list element]
  (let [r (reduce
            (fn [n x] (if (= x element) (reduced n) (inc n)))
            0 list)]
    (when (not= r (count list))
      r)))

;; I don't like it -- taking the count of a list is O(N)
;; try neg state, reduced change sign

(defn nindex-of  [coll element]
  (when (seq coll)
    (let [i (reduce (fn [n x] (if (= x element) (reduced (- n)) (inc n))) 0 coll)]
      (when-not (pos? i)
        (- i)))))


(defn nindex-of2  [coll element]
  (let [i (reduce (fn [n x] (if (= x element) (reduced (dec (- n))) (dec n))) -1 coll)]
    (when-not (neg? i)
      i)))


;; Question about the order of args?
(defn li-index-of [coll val]
  (first (keep-indexed #(when (= val %2) %1) coll)))


