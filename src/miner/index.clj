;; REALLY OLD, not reliable

;; SEM index-of should have coll first

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

