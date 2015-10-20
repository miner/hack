(ns miner.pairwise)


;; jnape09@gmail.com on Clojure mailing list
;; derived from < implementation
(defn pairwise?
  "Returns true if every sequential pair satisfies pred; false otherwise."
  {:inline         (fn [pred x y] `(~pred ~x ~y))
   :inline-arities #{3}}
  ([_ _] true)
  ([pred x y] (pred x y))
  ([pred x y & more]
   (if (pairwise? pred x y)
     (if (next more)
       (recur pred y (first more) (next more))
       (pairwise? pred y (first more)))
     false)))


;; juan.facorro@gmail.com response
;; partition return a lazy sequence and every? stops consuming on the first false result, so
;; this actually only process as many pairs as necessary.

;; whole coll arg
(defn pairwise-coll? 
  [pred coll] 
  (->> coll 
    (partition 2 1) 
    (every? (partial apply pred))))


;; SEM just to myself

(defn pwise?
  ([pred] true)
  ([pred one] true)
  ([pred x y] (pred x y))
  ([pred x y & more] (and (pred x y)
                          (apply pwise? pred y more))))



;; SEM might as well "optimize" the pred calls (basically the same as the original)
(defn pairwise2?
  "Returns true if every sequential pair satisfies pred; false otherwise."
  {:inline         (fn [pred x y] `(~pred ~x ~y))
   :inline-arities #{3}}
  ([_ _] true)
  ([pred x y] (pred x y))
  ([pred x y & more]
   (if (pred x y)
     (if (next more)
       (recur pred y (first more) (next more))
       (pred y (first more)))
     false)))

