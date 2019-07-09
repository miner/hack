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



(defn merge21
  ([fkey as] as)
  ([fkey as bs]
   (loop [as as bs bs res []]
     (cond (empty? as) (into res bs)
           (empty? bs) (into res as)
           :else (let [a (first as)
                       b (first bs)]
                   (if (pos? (compare (fkey a) (fkey b)))
                     (recur as (rest bs) (conj res b))
                     (recur (rest as) bs (conj res a))))))))


;; INCOMPLETE
#_
(defn mergez
  ([fkey as] as)
  ([fkey as bs]
   (loop [as as bs bs res []]
     (cond (empty? as) (into res bs)
           (empty? bs) (into res as)
           :else (let [a (first as)
                       b (first bs)]
                   (if (pos? (compare (fkey a) (fkey b)))
                     (recur as (rest bs) (conj res b))
                     (recur (rest as) bs (conj res a)))))))
  ([fkey as bs & more]
   ;; PROBABLY WRONG
   (recur (into [(mergez fkey as bs)] (apply mergez fkey more)))))





#_
(defn mapapp
  ([f] (map #(apply f %)))
  ([f a] (map #(apply f a %)))
  ([f a b] (map #(apply f a b %)))
  ([f a b & more] (map #(apply f a b (concat more %)))))

;;; Can we transduce across runs?
;;; (partion-all 2 runs)

(defn msort
  ([coll] (msort identity coll))
  ([fkey coll]
   (if (empty? coll)
     coll
     (loop [runs (mapv list coll)]
       (if (= (count runs) 1)
         (peek runs)
         (recur (into [] (comp (partition-all 2) (map #(apply merge21 fkey %))) runs)))))))

;;; could keep indexes or group counts and stay in one vector with subvecs sorted
;;; different virtual partitions 1,2,4,8...
;;; not worth the bookkeeping to reuse vector shell


;;; assume non-empty coll
(defn init-runs
  ([coll] (init-runs identity coll))
  ([fkey coll]
     (reduce (fn [r x] (if (neg? (compare (fkey x) (fkey (peek (peek r)))))
                         (conj r [x])
                         (conj (pop r) (conj (peek r) x))))
             [[(first coll)]]
             (rest coll))))

(defn partition-while [pred2 coll]
  (if (empty? coll)
    coll
    (reduce (fn [r x] (if (pred2 (peek (peek r)) x)
                        (conj (pop r) (conj (peek r) x))
                        (conj r [x])))
            [[(first coll)]]
            (rest coll))))

(defn part-while [pred2 coll]
  (if (empty? coll)
    coll
    (loop [part [(first coll)] cs (rest coll) res []]
      (if (empty? cs)
        (if (empty? part) res (conj res part))
        (if (pred2 (peek part) (first cs))
          (recur (conj part (first cs)) (rest cs) res)
          (recur [(first cs)] (rest cs) (conj res part)))))))
      

;; slightly faster to do the "natural" splits at the beginning.
(defn msa
  ([coll] (msa identity coll))
  ([fkey coll]
   (if (empty? coll)
     coll
     (loop [runs (partition-while #(not (pos? (compare (fkey %) (fkey %2)))) coll)]
       (if (= (count runs) 1)
         (peek runs)
         (recur (into [] (comp (partition-all 2) (map #(apply merge21 fkey %))) runs)))))))




(defn smoke-sort []
  (let [xs (repeatedly (long 1e6) #(rand-int 10000))]
    (assert (= (sort xs) (mergesort xs))))
  (let [amaps (shuffle (map #(hash-map :a % :b (- %)) (range 1000)))]
    (assert (= (sort-by :a amaps) (mergesort :a amaps)))
    (assert (= (sort-by :b amaps) (mergesort :b amaps))))
  true)



(def xs (repeatedly (long 1e6) #(rand-int 10000)))
(def mabs (shuffle (map #(hash-map :a % :b (- %)) (range 1000))))



