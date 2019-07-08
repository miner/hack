(ns miner.mergesort)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-334-tip-can-you-fill-in-the-blanks/

;; mergesort
;; https://en.wikipedia.org/wiki/Merge_sort
;;
;; Bonus:
;; Have it use compare to work on more than numbers.
;; Have it take a key to sort by.
;; Use clojure.reducers to implement a parallel version.

;; just for numbers
;; assume as and bs are already sorted and the same length

(defn merge-nums [as bs]
  (loop [as as bs bs res []]
    (cond (empty? as) (into res bs)
          (empty? bs) (into res as)
          :else (let [a (first as)
                      b (first bs)]
                  (if (<= a b)
                    (recur (rest as) bs (conj res a))
                    (recur as (rest bs) (conj res b)))))))


(defn mergesort-nums [coll]
  (if (empty? coll)
    coll
    (loop [runs (mapv list coll)]
      (if (= (count runs) 1)
        (peek runs)
        (recur (loop [[a b & cs] runs res []]
                 (cond (nil? a) res
                       (nil? b) (conj res a)
                       :else (recur cs (conj res (merge-nums a b))))))))))




(defn mergesort
  ([coll] (mergesort identity coll))
  ([fkey coll]
   (if (empty? coll)
     coll
     (let [merge2 (fn [as bs]
                    (loop [as as bs bs res []]
                      (cond (empty? as) (into res bs)
                            (empty? bs) (into res as)
                            :else (let [a (first as)
                                        b (first bs)]
                                    (if (pos? (compare (fkey a) (fkey b)))
                                      (recur as (rest bs) (conj res b))
                                      (recur (rest as) bs (conj res a)))))))]
       (loop [runs (mapv list coll)]
         (if (= (count runs) 1)
           (peek runs)
           (recur (loop [[a b & cs] runs res []]
                    (cond (nil? a) res
                          (nil? b) (conj res a)
                          :else (recur cs (conj res (merge2 a b))))))))))))



(defn smoke-sort []
  (let [xs (repeatedly (long 1e6) #(rand-int 10000))]
    (assert (= (sort xs) (mergesort xs))))
  (let [amaps (shuffle (map #(hash-map :a % :b (- %)) (range 1000)))]
    (assert (= (sort-by :a amaps) (mergesort :a amaps)))
    (assert (= (sort-by :b amaps) (mergesort :b amaps))))
  true)


