(ns miner.mergesort)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-334-tip-can-you-fill-in-the-blanks/

;; mergesort
;; https://en.wikipedia.org/wiki/Merge_sort
;;
;; Bonus:
;; Have it use compare to work on more than numbers.
;; Have it take a key to sort by.
;; Use clojure.reducers to implement a parallel version.

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


(defn partition-while
  "Returns a lazy sequence of partitions with each partition containing a run of elements for
  which `pred2` returns true when applied to the previous element and the current input.  The
  first input goes into the first partition without calling `pred2`.  Returns a stateful
  transducer when no collection is provided."
  ([pred2]
   (fn [rf]
     (let [a (java.util.ArrayList.)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
            (if (or (.isEmpty a) (pred2 (.get a (dec (.size a))) input))
                (do
                  (.add a input)
                  result)
                (let [v (vec (.toArray a))]
                  (.clear a)
                  (let [ret (rf result v)]
                    (when-not (reduced? ret)
                      (.add a input))
                    ret))))))))
  ([pred2 coll]
   (sequence (partition-while pred2) coll)))

;; slightly faster with some transducers
(defn xmergesort
  ([coll] (xmergesort identity coll))
  ([fkey coll]
   (if (empty? coll)
     coll
     (let [merge2 (fn
                    ([as] as)
                    ([as bs]
                     (loop [as as bs bs res []]
                       (cond (empty? as) (into res bs)
                             (empty? bs) (into res as)
                             :else (let [a (first as)
                                         b (first bs)]
                                     (if (pos? (compare (fkey a) (fkey b)))
                                       (recur as (rest bs) (conj res b))
                                       (recur (rest as) bs (conj res a))))))))
           lte (fn [a b] (not (pos? (compare (fkey a) (fkey b)))))]
       (loop [runs (into [] (partition-while lte) coll)]
         (if (= (count runs) 1)
           (peek runs)
           (recur (into [] (comp (partition-all 2) (map #(apply merge2 %))) runs))))))))


;; My reducers version wasn't faster so I didn't include it.


(defn smoke-sort []
  (let [xs (repeatedly (long 1e6) #(rand-int 10000))]
    (assert (= (sort xs) (mergesort xs) (xmergesort xs))))
  (let [amaps (shuffle (map #(hash-map :a % :b (- %)) (range 1000)))]
    (assert (= (sort-by :a amaps) (mergesort :a amaps) (xmergesort :a amaps)))
    (assert (= (sort-by :b amaps) (mergesort :b amaps) (xmergesort :b amaps))))
  true)




