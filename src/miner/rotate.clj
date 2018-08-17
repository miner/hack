(ns miner.rotate)


;; better fix in subvec.clj

;; work-around for subvector missing protocol support
(extend-type clojure.lang.APersistentVector$SubVector
  clojure.core.protocols/IKVReduce
  (kv-reduce [subv f init]
    (transduce (map-indexed vector)
               (fn ([ret] ret) ([ret [k v]] (f ret k v)))
               init
               subv)))

;; inspired by ray@1729.org.uk on clojure mailing list
;; by I always want vectors
;; for my usage, I don't care about lazy

;; slow lazy
(defn rotations0 [v]
  (let [n (count v)]
    (map vec (take n (partition n 1 (cycle v))))))


(defn rotations1 [pat]
  (reduce-kv (fn [res i x] (conj res (into [] (concat (subvec pat i) (subvec pat 0 i)))))
             nil
             pat))

;; much faster, but returns all subvectors
(defn rotations11 [pat]
  (reduce-kv (fn [res i x] (conj res (into (subvec pat i) (subvec pat 0 i))))
             nil
             pat))

;; guarantee real vectors, but big slowdown
(defn rotations12 [pat]
  (reduce-kv (fn [res i x] (conj res (into [] (into (subvec pat i) (subvec pat 0 i)))))
             nil
             pat))

;; slightly faster than 22
(defn rotations21 [pat]
  (loop [head () tail pat res nil]
    (if (seq tail)
      (recur (conj head (peek tail)) (pop tail) (conj res (into (vec head) tail)))
      res)))


(defn rotations22 [pat]
  (loop [head () tail pat res nil]
    (if (seq tail)
      (recur (conj head (peek tail)) (pop tail) (conj res (vec (concat head tail))))
      res)))

;; slower
(defn rotations23 [pat]
  (let [pp (cycle pat)
        cnt (count pat)]
    (map #(vec (take cnt (drop % pp))) (range cnt))))

;; super fast!
(defn rotations31
  "Returns seq of rotations of original vector pat.  Eager and fast.  Beware strangeness
  with subvectors."
  [pat]
  (let [pp (into pat pat)
        cnt (count pat)]
    (map #(subvec pp % (+ % cnt)) (range cnt))))

(defn rotations32
  "Returns seq of rotations of original vector pat.  Eager and fast.  Beware strangeness
  with subvectors."
  [pat]
  (let [pp (into pat pat)
        cnt (count pat)]
    (map #(into [] (subvec pp % (+ % cnt))) (range cnt))))

;; seq rotation, lazy
(defn rotate0
  ([s] (rotate0 1 s))
  ([n s] (let [cnt (count s)]
           (if (zero? cnt)
             (list (sequence s))
             (let [n (mod n cnt)]
               (concat (drop n s) (take n s)))))))

(defn rotate
  ([s] (rotate 1 s))
  ([n s] (let [cnt (count s)
               n (if (zero? cnt) 0 (mod n cnt))]
           (concat (drop n s) (take n s)))))

(defn rotations [s]
  (let [cnt (count s)]
    (if (zero? cnt)
      (list (sequence s))
      (take cnt (iterate rotate s)))))


(defn rot-test [rotf]
  (let [v10 (vec (range 10))]
    (apply = (map #(reduce + 0 %) (rotf v10)))))




;; from Apropos Clojure #15 on YouTube
;; They wanted a single rotation.  Most of the previous work in this file was trying to get
;; all rotations.  For the all-rotations problem, you really have to assume a finite coll.
;; But in general, it's better to use a lazy solution that doesn't need count.

;; classic, lazy solution:
(defn rotateAC [coll n]
  (concat (drop n coll) (take n coll)))

(defn rotateAC2 [coll n] (take (count coll) (drop n (cycle coll))))

;; SEM: Issue: using count is bad for infinite lists
;; to be safe, should check (counted? coll), or use (bounded-count ...)

;; my fix is to combine both, but is that really any better than simple rotateAC???
(defn rotateAC3 [coll n]
  (if (counted? coll)
    (take (count coll) (drop n (cycle coll)))
    (concat (drop n coll) (take n coll))))

(comment
  (take 5 (rotateAC (range) 2))
  ;;=> (2 3 4 5 6)
  
  (take 5 (rotateAC2 (range) 2))
  ;; never returns
  
