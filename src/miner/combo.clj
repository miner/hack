;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-318-tip-beware-the-order-of-keys-in-hashmaps/

;; Puzzle: generate combinations

;; If I have 4 flowers to choose from (#{:rose :lily :daisy :tulip}), I can generate 4
;; different combinations of 3 flowers.
;;
;; Write a function combinations that takes a collection of values and a number of items to
;; choose and generates all combinations of that size.
;;
;; (defn combinations [coll n] ...)
;;
;; (combinations #{:rose :lily :daisy :tulip} 3)
;; => (#{:rose :lily :daisy}, #{:rose :lily :tulip}, #{:rose :daisy :tulip}, #{:lily :daisy :tulip})


(ns miner.combo
  (:require [clojure.math.combinatorics :as mc]))

(def mc-combo mc/combinations)


;; for reference but not sure I need it
(defn range-permutations
  "Returns an eager sequence of vectors representing the permutations of the half-open
  range [0, N)."
  [n]
  {:pre [(not (neg? n))]}
  (reduce (fn [vs cnt]
            (reduce (fn [acc vvv]
                      (reduce-kv (fn [r i x] (conj r (assoc vvv i cnt cnt x)))
                                 (conj acc (conj vvv cnt))
                                 vvv))
                    ()
                    vs))
          (list [])
          (range n)))


;; NOT RIGHT!  need more swaps

;; pick a split point (n - 1) and then add last from rest






;; NOTE:  Eric wanted sets returned so you need one more level of (set ...)



;; SEM: idea -- think of mask walking across vector
;; generate the masks then convolve them
;; For n=3, three in a row, one-skip-three-four, one-skip-skip-four-five, 


(defn combo-indices-SAVE [cnt grp]
  ;; {:pre [(pos-int? grp) (<= grp cnt)]}
  (reduce (fn [res _]
            (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res))
          (map vector (range cnt))
          (range 1 grp)))

(defn combo-indices [cnt grp]
  ;; {:pre [(pos-int? grp) (<= grp cnt)]}
  (loop [i 1 res (map vector (range cnt))]
    (if (< i grp)
      (recur (inc i) (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res))
      res)))
  

;; WORKS slightly faster, maybe not as pretty
(defn combinations [coll n]
  (if-not (pos-int? n)
    (list ())
    (let [v (vec coll)
          cnt (count v)]
      (when (<= n cnt)
        (map #(map v %) (combo-indices cnt n))))))


(defn combinations0 [coll n]
  (if-not (pos-int? n)
    (list ())
    (let [v (vec coll)
          cnt (count v)]
      (when (<= n cnt)
        (sequence (map #(map v %)) (combo-indices cnt n))))))
          


(defn combinations1 [coll n]
  (if-not (pos-int? n)
    (list ())
    (let [v (vec coll)
          cnt (count v)]
      (when (<= n cnt)
        (map pop (reduce (fn [res i]
                           (mapcat (fn [prev] (map #(conj (pop prev) (v %) %)
                                                   (range (inc (peek prev)) cnt)))
                                   res))
                         (map vector v (range cnt))
                         (range 1 n)))))))

;; this range is just for driving the reduce, i is unused



;; BAD xtrans attempt
#_ (defn combinations2 [coll n]
  (if-not (pos-int? n)
    (list ())
    (let [v (vec coll)
          cnt (count v)]
      (when (<= n cnt)

        (transduce
         conj
         (fn
           ([res] (map pop res))
           ([res i] (mapcat (fn [prev] (map #(conj (pop prev) (v %) %)
                                                   (range (inc (peek prev)) cnt) prev)) res)))

         (map vector v (range cnt))
         (range 1 n))))))






(defn set= [a b] (= (set a) (set b)))

;; peek and inc to n add



(defn smoke-combo [cf]
  (assert (=  nil (cf [] 11)))
  (dotimes [n 5]
    (let [xs (range n)]
      (dotimes [m n]
        (assert (= (mc/combinations xs m) (cf xs m)) (str "Testing " m " with " n " items")))))
  true)


