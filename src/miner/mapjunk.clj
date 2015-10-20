

(defn vectorize [m] (reduce-kv (fn [ms k v] (conj ms {:id k :val v})) [] m))

(defn mapify [ms] (zipmap (map :id ms) (map :val ms)))

(defn mapify2 [ms] (into {} (map (juxt :id :val) ms)))


(defn mwm [xs ys] (vectorize (merge-with * (mapify xs) (mapify ys))))


;; keeps original map
(defn mapify-full [ms] (into {} (map (juxt :id identity) ms)))

(defn val* [m1 m2]
  (assert (= (:id m1) (:id m2)))
  (assoc m1 :val (* (:val m1) (:val m2))))

(defn mwmf [xs ys] (vals (merge-with val* (mapify-full xs) (mapify-full ys))))

(defn map-by [k ms] (into {} (map (juxt k identity) ms)))




(defn merge-key-with [k f & maps]
  (merge-with (fn [m1 m2] (assoc m1 k (f (get m1 k) (get m2 k)))) maps))

(defn mwmkf [xs ys] (vals (merge-key-with :val * (map-by :id xs) (map-by :id ys))))
