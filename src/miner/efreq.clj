(ns miner.efreq)

;;; Write a function that takes a collection and returns the most frequent element. But
;;; here's the thing: you can't use the built-in function clojure.core/frequencies. And if
;;; there are ties, just pick one.  return nil for an empty collection
;;; in the case of a tie, return one of the winners


(defn most-frequent [coll]
  (when-let [gs (seq (vals (group-by identity coll)))]
    (peek (apply max-key count gs))))

;; not bad
(defn gmf3 [coll]
  (when (seq coll)
    (->> coll
         (reduce (fn [m x] (assoc! m x (conj (get m x []) x))) (transient {}))
         persistent!
         vals
         (apply max-key count)
         peek)))

;; For reference, the natural way with `frequencies` doing most of the work
(defn fmf [coll]
  (when (seq coll)
    (key (apply max-key val (frequencies coll)))))


;; basically re-inventing `frequencies` which is not allowed
;; FASTEST
(defn fast-mf [coll]
  (when (seq coll)
    (->> coll
         (reduce (fn [tm x] (assoc! tm x (inc (get tm x 0)))) (transient {}))
         persistent!
         (apply max-key val)
         key)))













  
;; much slower
(defn mf1 [coll]
  (when (seq coll)
    (first (apply max-key count (partition-by identity (sort-by hash coll))))))


(defn mf [coll]
  (when (seq coll)
    (let [fq (reduce (fn [m x] (if (contains? m x)
                                 (update m x inc)
                                 (assoc m x 1)))
                     {}
                     coll)]
      (key (apply max-key val fq)))))




;; re-inventing `frequencies` so that's cheating
(defn mf3 [coll]
  (when (seq coll)
    (let [fq (persistent! (reduce (fn [m x] (assoc! m x (inc (get m x 0))))
                                  (transient {})
                                  coll))]
      (key (apply max-key val fq)))))


(defn mf4 [coll]
  (key (apply max-key val (persistent! (reduce (fn [m x] (assoc! m x (inc (get m x 0))))
                                               (transient {nil 0})
                                               coll)))))


;; {nil 0} saves a line of code, but is a bit slower 
(defn mf5 [coll]
  (->> coll
       (reduce (fn [tm x] (assoc! tm x (inc (get tm x 0)))) (transient {nil 0}))
       persistent!
       (apply max-key val)
       key))




(defn smoke-freq
 ([] (smoke-freq most-frequent))
 ([most-frequent]
  (assert (= (most-frequent [2 2 3 4 4 2 1 1 3 2]) 2))
  (assert (nil? (most-frequent [])))
  (assert (= (most-frequent [2 "two" "two" 2 2.0 2.0 "two"]) "two"))
  (assert (or (= (most-frequent [1 1 4 4 5])  4)
              (= (most-frequent [1 1 4 4 5])  1)))
  true))




(defn westcott-mf1 [xs]
  (->> xs (group-by identity) vals (sort-by (comp - count)) ffirst))

(defn westcott-mf2 [xs]
  (some->> xs (group-by identity) vals (apply max-key count) first))

(defn sztamas-mf [coll]
  (when (seq coll)
    (->>
      coll
      (group-by identity)
      (apply max-key (comp count second))
      first)))
