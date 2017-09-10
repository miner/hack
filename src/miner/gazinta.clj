(ns miner.gazinta)

;; Untested.   Needs performance evaluation.
;; Consider number and length of colls.  What's the typical application?  Really just
;; concat, but some special cases might need something better.

(defn gazinta
  ([] [])
  ([a] (vec a))
  ([a b] (persistent! (reduce conj! (transient (vec a)) b)))
  ([a b c] (persistent! (reduce #(reduce conj! % %2) (transient (vec a)) (list b c))))
  ([a b c d] (persistent! (reduce #(reduce conj! % %2) (transient (vec a)) (list b c d))))
  ([a b c d & more] (persistent! (reduce #(reduce conj! % %2)
                                         (transient (vec a))
                                         (list* b c d more)))))


(defn catall [& colls]
  (into [] (apply concat colls)))


(defn transcat [& colls]
  (transduce cat (completing conj! persistent!) colls))


(defn scat [& colls]
  (sequence cat colls))

(defn intos [& colls]
  (reduce into [] colls))



(defn goes-into
  ([] [])
  ([a] (vec a))
  ([a b] (into (vec a) b))
  ([a b c] (reduce into (vec a) (list b c)))
  ([a b c d] (reduce into (vec a) (list b c d)))
  ([a b c d & more] (reduce into (vec a) (list* b c d more))))



