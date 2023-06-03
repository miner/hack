(ns miner.ord-map
  [:require [tiara.data :as tia]])


;;; My hacking with Quoll's ordered-map, Tiara project
;;; https://github.com/quoll/tiara

;; quoll, fastest
(defn vreverse
  "Reverses a vector into a vector. Lists are reversed as usual."
  [v]
  (if (vector? v)
    (mapv #(nth v %) (range (dec (count v)) -1 -1))
    (reverse v)))


;; slower
(defn vrev1 [v]
  (if (vector? v)
    (into [] (rseq v))
    (reverse v)))

;; slower
(defn vrev [v]
  (if (vector? v)
    (persistent! (reduce conj! (transient []) (rseq v)))
    (reverse v)))


;;; guts of ordered-map for timing

(defn om-guts [& keyvals]
  (let [kv-vec (vreverse
                (second
                 (reduce
                  (fn [[seen? acc] [k v]]
                    (if (seen? k)
                      [seen? acc]
                      [(conj seen? k) (conj acc (clojure.lang.MapEntry/create k v))]))
                  [#{} []] (reverse (partition 2 keyvals)))))]
    (apply hash-map (interleave (map first kv-vec) (range)))))



;;; SEM hack
(defn omg [& keyvals]
  (let [kv-vec (vreverse
                (peek
                 (reduce
                  (fn [[seen? acc] [k v]]
                    (if (seen? k)
                      [seen? acc]
                      [(conj seen? k) (conj acc (clojure.lang.MapEntry/create k v))]))
                  [#{} []]
                  (reverse (partition 2 keyvals)))))]
    (apply hash-map (interleave (map #(nth % 0) kv-vec) (range)))))



(defn omkv [& keyvals]
  (vreverse
   (second
    (reduce (fn [[seen? acc] [k v]]
              (if (seen? k)
                [seen? acc]
                [(conj seen? k) (conj acc (clojure.lang.MapEntry/create k v))]))
            [#{} []]
            (reverse (partition 2 keyvals))))))

;; slower with list vs vec
(defn lomkv [& keyvals]
  (vreverse
   (second
    (reduce (fn [[seen? acc :as res] [k v]]
              (if (seen? k)
                res
                (list (conj seen? k) (conj acc (clojure.lang.MapEntry/create k v)))))
            (list #{} [])
            (reverse (partition 2 keyvals))))))

;; faster with partitionv
(defn somkv [& keyvals]
  (vreverse
   (peek
    (reduce (fn [[seen? acc] [k v]]
              (if (seen? k)
                [seen? acc]
                [(conj seen? k) (conj acc (clojure.lang.MapEntry/create k v))]))
            [#{} []]
            (reverse (partitionv 2 keyvals))))))

;;; :as not faster
(defn somkv2 [& keyvals]
  (vreverse
   (peek
    (reduce (fn [[seen? acc :as res] [k v]]
              (if (seen? k)
                res
                [(conj seen? k) (conj acc (clojure.lang.MapEntry/create k v))]))
            [#{} []]
            (reverse (partitionv 2 keyvals))))))

;;; notice that the last entry value is preferred but the position is determined by first
;;; mention -- which matters for duplicate keys.  That's the reason for the apparent double
;;; reverses.

(defn momkv [& keyvals]
  (let [m (apply hash-map keyvals)]
    (mapv #(find m %) (if (= (* 2 (count m)) (count keyvals))
                        (take-nth 2 keyvals)
                        (reverse (distinct (reverse (take-nth 2 keyvals))))))))


(defn xomkv [& keyvals]
  (let [m (apply hash-map keyvals)]
    (if (= (* 2 (count m)) (count keyvals))
      (into [] (comp (take-nth 2) (map #(find m %))) keyvals)
      (into [] (into () (comp (distinct) (map #(find m %))) (into () (take-nth 2) keyvals))))))




;; canonical, like distinct but prefers later occurrence 
(defn redist [xs]
  (reverse (distinct (reverse xs))))


;;; a bit faster on shortish data, not sure it's worth it
(defn edist [xs]
  (::tail-ord (reduce (fn [fqs x]
            (if (= (fqs x) 1)
              (update fqs ::tail-ord conj x)
              (update fqs x dec)))
          (assoc (frequencies xs) ::tail-ord [])
          xs)))


(defn distx [xs]
  (if (apply distinct? xs)
    xs
    (reverse (distinct (reverse xs)))))

(defn dupes
  "Returns the set of duplicates in coll."
  [coll]
  (loop [seen #{} dups #{} coll coll]
    (if-let [cs (seq coll)]
      (let [item (first cs)]
        (if (contains? seen item)
          (recur seen (conj dups item) (rest cs))
          (recur (conj seen item) dups (rest cs))))
      dups)))

      
      


(defonce k50 (conj (range 100) 50 50))

(defonce kc4 (interleave (cycle [1 2 3 4]) (range 100)))

(defonce data (shuffle (range 1000000)))




(defn ORIG-ord-map
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] tia/EMPTY_MAP)
  ([& keyvals]
   (let [kv-vec (vreverse
                  (second
                    (reduce
                      (fn [[seen? acc] [k v]]
                        (if (seen? k) [seen? acc]
                            [(conj seen? k) (conj acc (clojure.lang.MapEntry/create k v))]))
                      [#{} []] (reverse (partition 2 keyvals)))))]
     (tiara.data.VecMap.
       kv-vec
       (apply hash-map (interleave (map first kv-vec) (range)))))))

;; faster saving ks order, avoiding laziness
(defn ord-map
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] tia/EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         ks (if (= (* 2 (count m)) (count keyvals))
              (into [] (take-nth 2) keyvals)
              (into () (distinct) (into () (take-nth 2) keyvals)))]
     (tiara.data.VecMap.
       (mapv #(find m %) ks)
       (zipmap ks (range))))))

;;; benchmark with something like this
#_ (quick-bench (apply ord-map data))


;; slower
(defn ord-map1
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] tia/EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         kv-vec (if (= (* 2 (count m)) (count keyvals))
                  (into [] (comp (take-nth 2) (map #(find m %))) keyvals)
                  (into [] (into () (comp (distinct) (map #(find m %)))
                                 (into () (take-nth 2) keyvals))))]
     (tiara.data.VecMap.
       kv-vec
       (apply hash-map (interleave (map #(nth % 0) kv-vec) (range)))))))

;; slower with lazy take-nth
(defn ord-map2
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] tia/EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         ks (if (= (* 2 (count m)) (count keyvals))
              (take-nth 2 keyvals)
              (into () (distinct) (into () (take-nth 2) keyvals)))
         kv-vec (into [] (map #(find m %)) ks)]
     (tiara.data.VecMap.
       kv-vec
       (zipmap ks (range))))))


(defn ord-map3
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] tia/EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         ks (if (= (* 2 (count m)) (count keyvals))
              (into [] (take-nth 2) keyvals)
              (into () (distinct) (into () (take-nth 2) keyvals))) ]
     (tiara.data.VecMap.
       (into [] (map #(find m %)) ks)
       (zipmap ks (range))))))


;; not faster with map-indexed but about same
(defn ord-map4
  "Creates a map object that remembers the insertion order, similarly to a java.util.LinkedHashMap"
  ([] tia/EMPTY_MAP)
  ([& keyvals]
   (let [m (apply hash-map keyvals)
         ks (if (= (* 2 (count m)) (count keyvals))
              (into [] (take-nth 2) keyvals)
              (into () (distinct) (into () (take-nth 2) keyvals))) ]
     (tiara.data.VecMap.
      (mapv #(find m %) ks)
      (into {} (map-indexed (fn [i k] [k i]) ks))))))
