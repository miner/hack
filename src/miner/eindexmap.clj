(ns miner.eindexmap)

;;; https://gist.github.com/ericnormand/f38b0ecda60ebe51b1ac4395b8db3b6a

;;; Write a function that takes a sequence and returns a map where the elements of the
;;; sequence are the keys and the values are sets of the indexes where the value is found.


;; fastest
(defn index-map [coll]
  (persistent!
   (reduce-kv (fn [tm i k] (assoc! tm k (conj (get tm k #{}) i)))
              (transient {})
              (vec coll))))


(defn index-map1 [coll]
  (apply merge-with into {} (map #(hash-map % (hash-set %2)) coll (range))))


(defn index-map0 [coll]
  (let [sconj (fnil conj #{})]
    (reduce (fn [m [k v]] (update m k sconj v))
            {}
            (map vector coll (range)))))


(defn index-map2 [coll]
  (let [sconj (fnil conj #{})]
    (reduce (fn [m [k v]] (update m k sconj v))
            {}
            (map-indexed #(vector %2 %) coll))))


(defn index-map21 [coll]
  (let [sconj (fnil conj #{})]
    (reduce (fn [m [v k]] (update m k sconj v))
            {}
            (map-indexed vector coll))))


;; almost
(defn index-map3 [coll]
  (transduce (map-indexed vector)
             (completing (fn [tm [v k]] (assoc! tm k (conj (get tm k #{}) v)))
                         persistent!)
             (transient {})
             coll))







(defn smoke-ind
  ([] (smoke-ind index-map))
  ([index-map]
   (assert (= (index-map [])  {}))
   (assert (= (index-map [1 2 3])  {1 #{0} 2 #{1} 3 #{2}}))
   (assert (= (index-map [1 1 1])  {1 #{0 1 2}}))
   (assert (= (index-map [1 2 1 2 1]) {1 #{0 2 4} 2 #{1 3}}))
   true))

;; Bonus: Write the inverse function that takes one of the returned maps and turns it into a
;; sequence.


;;; i is an index (int) so the sorted-map guarantees the order of vals


(defn inv-index-map [m]
  (-> (reduce-kv (fn [sm k is] (reduce (fn [sm i] (assoc sm i k)) sm is))
                 (sorted-map)
                 m)
      vals
      vec))


(defn inv-index-map1 [m]
  (-> (reduce (fn [sm [k is]] (reduce (fn [sm i] (assoc sm i k)) sm is))
              (sorted-map)
              m)
      vals
      vec))



;; not so fast
(defn invim4 [m]
  (mapv val (sort-by key
                     (persistent!
                      (reduce (fn [tm [k is]] (reduce (fn [tm i] (assoc! tm i k)) tm is))
                              (transient {})
                              m)))))
                


;; faster but ugly
(defn invim1 [m]
  (persistent!
   (reduce-kv (fn [tv k is]
                (reduce (fn [tv i]
                          (let [cnt (count tv)
                                tv (if (<= i cnt)
                                     tv
                                     (reduce conj! tv (repeat (- i cnt) nil)))]
                            (assoc! tv i k)))
                        tv
                        is))
              (transient [])
              m)))


;; slower than invim1
(defn invim2 [m]
  (persistent!
   (reduce-kv (fn [tv k is]
                (let [mx (apply max is)
                      cnt (count tv)]
                (reduce (fn [tv i] (assoc! tv i k))
                        (if (> mx cnt)
                          (reduce conj! tv (repeat (- mx cnt) nil))
                          tv)
                        is)))
              (transient [])
              m)))

;; slower
(defn invim3 [m]
  (let [mx (reduce + 0 (map count (vals m)))]
    (vec (reduce-kv (fn [arr k is]
                 (reduce (fn [arr i] (aset-long arr i k) arr)
                         arr
                         is))
               (long-array mx)
               m))))


(defn smoke-inv
  ([] (smoke-inv inv-index-map))
  ([inv-index-map]
   (assert (= [] (inv-index-map {})))
   (assert (= [1 2 3] (inv-index-map  {1 #{0} 2 #{1} 3 #{2}})))
   (assert (= [1 1 1] (inv-index-map  {1 #{0 1 2}})))
   (assert (= [1 2 1 2 1] (inv-index-map {1 #{0 2 4} 2 #{1 3}})))
   true))




;; @mchampine -- short but slow
(defn mc-index-map [s]
  (into {} (for [[k v] (group-by second (map-indexed vector s))] [k (set (map first v))])))

(defn mc-un-index-map [m]
  (mapv first (sort-by second (apply concat (for [[k v] m] (map vector (repeat k) v))))))

;; @steffan-westcott 
(defn sw-index-map [xs]
  (reduce-kv (fn [m idx x] (update m x (fnil conj #{}) idx)) {} xs))

;; SEM added vec to fix nil result
(defn sw-invert-index-map [m]
  (vec (vals (reduce-kv (fn [m' x idxs] (merge m' (zipmap idxs (repeat x)))) (sorted-map) m))))
