(ns miner.zipconj)

(defn zipconj
  ([keys vals] (zipconj keys vals []))
  ([keys vals empty-val]
   (reduce (fn [res kv] (update res (first kv) (fnil conj empty-val) (second kv)))
           {}
           (map list keys vals))))


(defn zipconj1
  ([keys vals] (zipconj1 keys vals []))
  ([keys vals empty-val]
   (reduce (fn [res kv] (assoc res (first kv) (conj (get res (first kv) empty-val) (second kv))))
           {}
           (map list keys vals))))

;; YES, faster
(defn zipconj2
  ([keys vals] (zipconj2 keys vals []))
  ([keys vals empty-val]
   (persistent! (reduce (fn [res kv]
                          (assoc! res (first kv)
                                  (conj (get res (first kv) empty-val) (second kv))))
                        (transient {})
                        (map list keys vals)))))

;; Note:  sequence (map list) ... is much slower

(defn zipconj3
  ([keys vals] (zipconj3 keys vals []))
  ([keys vals empty-val]
   (persistent! (reduce (fn [res kv]
                          (assoc! res (first kv)
                                  (conj (get res (first kv) empty-val) (second kv))))
                        (transient {})
                        (sequence (map list) keys vals)))))


;; could also try transient/assoc!/persistent! to speed up slightly

;; NOT FASTER
(defn tzipconj
  ([keys vals] (tzipconj keys vals []))
  ([keys vals empty-val]
   (transduce identity
              (completing (fn [res kv] (assoc res (first kv) (conj (get res (first kv) empty-val) (second kv)))))
           {}
           (sequence (map list) keys vals))))

(defn tzc
  ([keys vals] (tzc keys vals []))
  ([keys vals empty-val]
   (reduce (fn [res kv] (assoc res (first kv) (conj (get res (first kv) empty-val) (second kv))))
           {}
           (sequence (map list) keys vals))))




;; similar to clojure.set/map-invert but allows duplicates so result vals are collections.
;; empty-val determines the type (default vector).
(defn invmap
  ([m] (invmap m []))
  ([m empty-val]
   (reduce-kv (fn [res k v] (assoc res v (conj (get res v empty-val) k)))
           {}
           m)))

;; You can't be lazy when result has to be a map.  Lazy only makes sense with a sequence result.

;; What would a lazy map be?  Can't be sorted.  Seq order has to be the conj/creation order.
;; Keep parallel vectors of keys and values, plus map for quick access.  Not really lazy if
;; you need the map.


;; slice-by like partition-by but expects singleton items satisfying pred, then conjoins
;; following items after it.  If coll doesn't start with a pred item, nil is placed in the
;; first place to mark potential junk.  When given an empty collection, just returns nil --
;; no false-start segment.

;; faster and simpler to reduce, but not lazy

(defn slice-by
  ([pred coll] (slice-by pred coll nil))
  ([pred coll false-start]
   (reduce (fn [res x]
             (if (pred x)
               (conj res [x])
               (conj (pop res) (conj (peek res) x))))
           (when-first [fst coll]
             (if (pred fst) [[fst]] [[false-start fst]]))
           (rest coll))))

;; SEM FIXME: the other attempts don't necessarily had the false-start and empty collection
;; cases.  That would need to be fixed.

(defn tslice-by
  ([pred]
  (fn [rf]
    (let [a (java.util.ArrayList.)]
      (fn
        ([] (rf))
        
        ([result]
           (let [result (if (.isEmpty a)
                          result
                          (let [v (vec (.toArray a))]
                            ;;clear first!
                            (.clear a)
                            (unreduced (rf result v))))]
             (rf result)))

        ([result input]
         (if (pred input)
           (if (.isEmpty a)
             (do (.add a input)
                 result)
             (let [v (vec (.toArray a))]
               (.clear a)
               (let [ret (rf result v)]
                   (when-not (reduced? ret)
                     (.add a input))
                   ret)))
           (do
             (when (.isEmpty a)
               (.add a nil))
             (.add a input)
             result) ))))))
  ([pred coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (let [fst (first s)
              run1 (cons fst (take-while (complement pred) (next s)))
              run (if (pred fst) run1 (cons nil run1))]
          (cons run (tslice-by pred (seq (drop (count run1) s)))))))))

;; BAD
(defn WAS-slice-by [pred coll]
  (let [parts (partition-by pred coll)]
    (map #(cons (ffirst %) (second %))
         (partition-all 2  (if (pred (first coll)) parts (cons (list nil) parts))))))


(comment BUGGY
(islice-by zero? [ 0 0 1 2 3 0 4  5 6 0 7])
[(0 1 2 3) (0 4 5 6) (0 7)]
user=> (slice-by zero? [ 0 0 1 2 3 0 4  5 6 0 7])
[[0] [0 1 2 3] [0 4 5 6] [0 7]]
)

;; BUGGY
(defn islice-by [pred coll]
  (let [false-start nil
        false-run (take-while (complement pred) coll)]
    (into (if (empty? false-run) [] [(concat [false-start] false-run)])
          (comp (partition-by pred) (partition-all 2) (map #(cons (ffirst %) (second %))))
          (drop (count false-run) coll))))

;; but doesn't handle false start
