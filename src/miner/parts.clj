(ns miner.parts
  (:require [clojure.math.combinatorics :as mc]))


;; Mailing list question:
;; Re: Combinatorics partitions that preserves adjacency?

;; Does anyone know of a straightforward way to get something like
;; clojure.math/combinatorics/partitions that works more like partition in the core library,
;; that is, that only selects partitions with adjacent elements?

;; Answer from: Mikera <mike.r.anderson.13@gmail.com>
(defn ordered-partitions1 
 ([[fst & more]]
   (ordered-partitions1 (list [fst]) more))
 ([init more]  
   (if (empty? more)
      (list init)
      (let [more-partitions (ordered-partitions1 more)
            start (butlast init)
            join (last init)]
        (concat
          (map #(concat init %) more-partitions)
          (map #(let [[more-fst & more-more] %]
                  (concat start (list (vec (concat join more-fst))) more-more)) more-partitions))))))

(comment
(time (count (ordered-partitions1 (range 20))))
"Elapsed time: 822.939961 msecs"
524288
)



;; Mark Engelberg <mark.engelberg@gmail.com> says use non-empty mc/subsets to figure out
;; partitions.  Here's my implementation:

;; Note: this depends on stable subsets, preserving the order of the original collection.

(defn subv-parts [coll]
  (let [v (vec coll)
        cnt (count v)]
    (map (fn [splits]
           (map (fn [start end] (subvec v start end))
                (conj splits 0)
                (concat splits (list cnt))))
         (mc/subsets (range 1 cnt)))))



(defn set= [as bs]
  (= (set as)
     (set bs)))



;; My good solution.  By far faster than others.

;; inclusive sizes
(defn sized-subsets [items min-count max-count]
  (mapcat (fn [n] (mc/combinations items n))
          (range min-count (inc max-count))))


(defn subv-ordered-partitions [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        smin (dec (or from 1))
        smax (dec (or to cnt))]
    (map (fn [splits]
           (map (fn [start end] (subvec v start end))
                (conj splits 0)
                (concat splits (list cnt))))
         (sized-subsets (range 1 cnt) smin smax))))



;; inline the sized-subsets and fiddle the limits
(defn sem-ordered-partitions [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        r1 (range 1 cnt)]
    (map (fn [splits]
           (map (fn [start end] (subvec v start end))
                (conj splits 0)
                (concat splits (list cnt))))
         (mapcat (fn [n] (mc/combinations r1 n))
                 (range (dec (or from 1)) (or to cnt))))))


;; how about a transducer version???

;; works but not faster
(defn trans-ordered-partitions1 [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        r1 (range 1 cnt)]
    (sequence 
     (comp (mapcat (fn [n] (mc/combinations r1 n)))
           (map (fn [splits] (concat (conj splits 0) (list cnt))))
           (map #(partition 2 1 %))
           (map #(map (fn [[start end]] (subvec v start end)) %)))
     (range (dec (or from 1)) (or to cnt)))))

;; slightly faster when not lazy
(defn trans-ordered-partitions2 [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        r1 (range 1 cnt)]
    (into ()
     (comp (mapcat (fn [n] (mc/combinations r1 n)))
           (map (fn [splits] (concat (conj splits 0) (list cnt))))
           (map #(partition 2 1 %))
           (map #(map (fn [[start end]] (subvec v start end)) %)))
     (range (dec (or from 1)) (or to cnt)))))




(defn slow-trans-ordered-partitions [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        r1 (range 1 cnt)]
    (into []
     (comp (mapcat (fn [n] (mc/combinations r1 n)))
           (map (fn [splits]
                   (peek (reduce (fn [[start r] end]
                                  [end (conj r (subvec v start end))])
                                [0 []]
                                (concat splits (list cnt)))))))

     (range (dec (or from 1)) (or to cnt)))))

(defn slow2-trans-ordered-partitions [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        r1 (range 1 cnt)]
    (into []
     (comp (mapcat (fn [n] (mc/combinations r1 n)))
           (map (fn [splits]
                  (loop [starts (into [0] splits) end cnt res nil]
                    (if (seq starts)
                      (recur (pop starts)
                             (peek starts)
                             (conj res (subvec v (peek starts) end)))
                      res)))))

     (range (dec (or from 1)) (or to cnt)))))



;;; Paul Gowder <paul.gowder@gmail.com> solution:  (Slow)
(defn breaks->partition 
 ([v brks]
  (breaks->partition 0 [] v brks))
 ([start pars v brks]
  (if (empty? brks)
    (conj pars (subvec v start (count v)))
    (let [this-part (subvec v start (first brks))]
      (recur (first brks) (conj pars this-part) v (rest brks))))))

(defn min-parts [min splits]
 (>= (count splits) (- min 1)))

(defn max-parts [max splits]
 (<= (count splits) (- max 1)))

(defn ordered-partitions [v & {:keys [max min]}]
 (let 
   [s (c/subsets (range 1 (count v)))
    fs (cond
         (and max min) 
         (filter 
           (partial max-parts max) 
           (filter (partial min-parts min) s))
         max (filter (partial max-parts max) s)
         min (filter (partial min-parts min) s)
         :else s)]
   (map (partial breaks->partition v) fs)))
