;; http://ericasadun.com/2017/01/24/swift-idioms/


;; wikipedia definition: The mode is the value that appears most often in a set of data. The
;; mode of a discrete probability distribution is the value x at which its probability mass
;; function takes its maximum value. In other words, it is the value that is most likely to be
;; sampled.

;; data.priority-map would also help with this


(def xs [1 1 5 7 9 23 2 4 5 5])

;;  (frequencies xs)
;;  => {1 2, 5 3, 7 1, 9 1, 23 1, 2 1, 4 1}
;;
;; mode is 5 with count 3, or [5 3]

(defn mode1 [xs]
  (first (sort-by (comp - val) (frequencies xs))))

(defn mode2 [xs]
  (first (sort-by val (fn [a b] (compare b a)) (frequencies xs))))

(defn mode3 [xs]
  (reduce-kv (fn [max-kv k v] (if (> v (peek max-kv)) [k v] max-kv)) [nil 0] (frequencies xs)))

(defn mode4 [xs]
  (reduce (fn [max-kv kv] (if (> (peek kv) (peek max-kv)) kv max-kv)) [nil 0] (frequencies xs)))

(defn max-kv [f m]
  (apply max-key f m))
  
(defn mode0 [xs]
  (max-kv val (frequencies xs)))

(defn mode [xs]
  (apply max-key val (frequencies xs)))



;; Just for inspection. A simplified version.  Actual `frequencies` uses transients for performance.
(defn freqs
  "Returns a map from distinct items in coll to the number of times
  they appear."
  [coll]
  (reduce (fn [counts x] (assoc counts x (inc (get counts x 0))))
          {}
          coll))

