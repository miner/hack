(ns miner.esocks)

;; https://gist.github.com/ericnormand/41680eb7155151f903947768e420891e
;;
;; Write a function that takes a collection of values and makes pairs if they are equal. If
;; there is not a match for a particular value, return it as well.

(defn pair-match [socks]
  (reduce-kv (fn [m k cnt] (-> m
                               (update :pairs into (repeat (quot cnt 2) [k k]))
                               (cond-> (odd? cnt) (update :unmatched conj k))))
               {:pairs [] :unmatched []}
           (frequencies socks)))





(defn kinto [m k xform xs]
  (assoc m k (into (get m k) xform xs)))

(defn pair-match2 [socks]
  (reduce-kv (fn [m k socks] (let [cnt (count socks)]
                               (if (even? cnt)
                                 (kinto m :pairs (partition-all 2) socks)
                                 (-> m
                                     (xinto :pairs (partition-all 2) (rest socks))
                                     (update :unmatched conj k)))))
               {:pairs [] :unmatched []}
           (group-by identity socks)))



(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))

(defn psort= [pa pb]
  (and (= (sort (vec (:pairs pa))) (sort (vec (:pairs pb))))
       (= (sort (vec (:unmatched pa))) (sort (vec (:unmatched pb))))))

(defn smoke-socks [pair-match]
  (assert? psort=
           (pair-match []) {:pairs [] :unmatched []}
           (pair-match [1 2 1]) {:pairs [[1 1]] :unmatched [2]}
           (pair-match [1 2 3 1 2 3 1 1 2]) {:pairs [[1 1] [2 2] [3 3] [1 1]]  :unmatched [2]}))

                                 
