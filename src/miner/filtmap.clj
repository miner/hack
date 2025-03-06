(ns miner.filtmap)


;;; original
;;; https://clojure-diary.gitlab.io/2025/03/04/filtering-maps-based-on-key-value-pairs-in-clojure.html



(defn key-has-value? [key value map]
  (= value (get map key)))

(defn map-filter [key value seq-of-maps]
  (filter #(key-has-value? key value %) seq-of-maps))

(defn maps-having [filter-map seq-of-map]
  (if (empty? filter-map)
    seq-of-map
    (let [[key value] (first filter-map)
          filtered-seq (map-filter key value seq-of-map)]
      (recur (rest filter-map) filtered-seq))))


;; let's test the above code

(def users [{"name" "Karthik", "country" "India"}
            {"name" "Pari", "country" "India"}
            {"name" "John", "country" "England"}])

(comment

(key-has-value? "country" "India" {"name" "Karthik", "country" "India"})

(map-filter "country" "India" users)

(maps-having {"country" "India" "name" "Karthik"} users)

)



;;; SEM version.  I think this is the natural implementation.
(defn submap? [sm target]
  (reduce-kv (fn [res k v]
               (if (= (get target k) v)
                 res
                 (reduced false)))
             true
             sm))

(defn mhaving [sm ms]
  (filter #(submap? sm %) ms))



;; slightly faster, but probably not worth the complexity
(defn smap-fn [sm]
  (reduce-kv (fn [pred k v]
               (fn [m] (when (pred m) (= (get m k) v))))
             any?
             sm))

(defn mhav2 [sm ms]
  (filter (smap-fn sm) ms))



#_
(mhaving {"country" "India" "name" "Karthik"} users)


(def users2 (into [{:xxx 1}
                   {:xxx 2}
                   {:yyy 3}
                   {:zzz 4}
                   {"name" "Karthik", "country" "India"}
                   {"name" "Karthik", "country" "India" :xxx 1}
                   {"name" "Karthik", "country" "India" :xxx 2}
                   {"name" "Pari", "country" "India"}
                   {"name" "Pari", "country" "India" :xxx 1}
                   {"name" "Pari", "country" "India" :xxx 2}
                   {"name" "Pari", "country" "India" :xxx 3}
                   {"name" "John", "country" "England"}]
                  (map #(hash-map "name" %))
                  (range 100)))

