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

;; You can't be lazy when result has to be a map.  It has to be a sequence out.

;; What would a lazy map be?  Can't be sorted.  Seq order has to be the conj/creation order.
;; Keep parallel vectors of keys and values, plus map for quick access.  Not really lazy if
;; you need the map.


;; slice-by like partition-by but expects singleton items satisfying pred, then conjoins
;; following items after it.  If coll doesn't start with a pred item, nil is placed in the
;; first place to mark potential junk.

;; faster and simpler to reduce, but not lazy
(defn slice-by [pred coll]
  (reduce (fn [res x]
            (if (pred x)
              (conj res [x])
              (if (empty? res)
                [[nil x]]
                (conj (pop res) (conj (peek res) x)))))
          []
          coll))


;; like partition-by but takes a predicate that indicates the start of a grouping.  Anything
;; that fails is added to the current group.  If first thing fails, nil is used as a marker.

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

