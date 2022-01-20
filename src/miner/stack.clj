
;; note: Cons, Range, LazySeq (results from map, filter, etc.) are not stacks
;; concrete Vector, List and Queue are stacks
(defn stack? [x]
  ;; works with peek and pop
  (instance? clojure.lang.IPersistentStack x))

;; If you need a stack, use `vec` to convert a possibly lazy collection but remember the
;; peek "head" is the last element for a vector.  If you want a list, you have to make
;; it as in `as-list` below.

(defn stack [coll]
  (vec coll))

(defn as-list [coll]
  ;; returns a concrete PersistentList, usuable as a stack
  (if (list? coll)
    coll
    (apply list coll)))





;;----------------------------------------------------------------------

;; Not a good idea, better to use real reduce and consider your source collection
(defn poppin-stack [f init stack]
  (loop [res init st stack]
    (if (empty? st)
      res
      (recur (f res (peek st)) (pop st)))))

;; still not a good idea
(defn reduce-stack [f init stack]
  (reduce f init (if (vector? stack) (rseq stack) stack)))

;; Note: rseq is fast on vector so it's a good trick to remember
