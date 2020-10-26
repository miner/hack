(ns miner.esumpair
  (:require [clojure.math.combinatorics :as mc]))


;; https://gist.github.com/ericnormand/c2c94f698bf3ace64c5f722da6dec2fc

;; Write a function that takes a collection of numbers and a target number. Return all pairs
;; of numbers found in the collection that sum up to the target number.  There can be
;; duplicate numbers and hence duplicate pairs. Each pair should be sorted.


;; Note: mc/combinations doesn't reuse duplicate items so you need to generate the indices
;; to be unique.  As we only need a pair of indices, the `for` expr is a natural way to go.


;; clean and simple.  I think it's faster to sort coll first, rather than each result.
(defn sums-of-pairs [coll sum]
  (let [vvv (vec (sort coll))
        cnt (count vvv)]
    (for [i (range cnt)
          j (range (inc i) cnt)
          :when (= sum (+ (vvv i) (vvv j)))]
      [(vvv i) (vvv j)])))


;; slightly faster, but more obscure
(defn fast-sums-of-pairs [coll sum]
  (let [vvv (vec (sort coll))
        cnt (count vvv)]
    (into [] (mapcat (fn [i] (keep (fn [j] (when (= sum (+ (vvv i) (vvv j)))
                                             (vector (vvv i) (vvv j))))
                                   (range (inc i) cnt))))
          (range cnt))))




;; natural way
(defn sums-of-pairs1 [coll sum]
  (let [vvv (vec (sort coll))]
    (keep (fn [[i j]] (when (= sum (+ (vvv i) (vvv j)))
                        (vector (vvv i) (vvv j))))
            (mc/combinations (range (count vvv)) 2))))


;; eager, not lazy, faster than using clojure.math.combinatorics/combinations
(defn combo-indices [cnt choose]
  ;; {:pre [(pos-int? choose) (<= choose cnt)]}
  (loop [i (unchecked-dec choose) res (map vector (range cnt))]
    (if (zero? i)
      res
      (recur (unchecked-dec i)
             (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res)))))


(defn sums-of-pairs2 [coll sum]
  (let [vvv (vec (sort coll))]
    (keep (fn [[i j]] (when (= sum (+ (vvv i) (vvv j)))
                        (vector (vvv i) (vvv j))))
          (combo-indices (count vvv) 2))))

;; we only need index pairs so we can simplify and use a transducer chain

;; specialized version of combo-indices for exactly 2
(defn indices2 [cnt]
  ;; {:pre [(>= cnt 2)]}
  (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt)))
          (map vector (range cnt))))



;; errors in examples
;; should sort results so [0 5] comes before [2 3]
;; third example should return [2 5] as well

(defn smoke-sum
  ([] (smoke-sum sums-of-pairs))
  ([sums-of-pairs]
   (assert (= (sums-of-pairs [2 4 5 6] 8) [[2 6]]))
   (assert (= (sums-of-pairs [3 2 0 1 1 5] 5) [[0 5] [2 3]]))
   (assert (= (sums-of-pairs [1 3 2 3 4 5] 7) [[2 5] [3 4] [3 4]]))
   true))



