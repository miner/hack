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


(defn iter [f x n]
  (if-not (pos-int? n)
    x
    (loop [i n res x]
      (if (zero? i)
        res
        (recur (unchecked-dec i) (f res))))))


;; slower, probably iter overhead
(defn combo-indices-ITER [cnt choose]
  (iter (fn [res]
          (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res))
        (map vector (range cnt))
        choose))


(defn combo-indices-SAVE [cnt grp]
  ;; {:pre [(pos-int? grp) (<= grp cnt)]}
  (reduce (fn [res _]
            (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res))
          (map vector (range cnt))
          (range 1 grp)))

(defn combo-indices-FAST [cnt choose]
  ;; {:pre [(pos-int? choose) (<= choose cnt)]}
  (loop [i 1 res (map vector (range cnt))]
    (if (< i choose)
      (recur (inc i) (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res))
      res)))



(defn combo-indices [cnt choose]
  ;; {:pre [(pos-int? choose) (<= choose cnt)]}
  (loop [i (unchecked-dec choose) res (map vector (range cnt))]
    (if (zero? i)
      res
      (recur (unchecked-dec i)
             (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res)))))


;; WORKS slightly faster, maybe not as pretty
(defn combinations-BEST [coll n]
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




;; integrated combo-indices, hoping for speed, but didn't pay off in my testing
;; and is less readable
(defn combinations5 [coll n]
  (if-not (pos-int? n)
    (list ())
    (let [v (vec coll)
          cnt (count v)]
      (when (<= n cnt)
        (loop [i (unchecked-dec n) res (map vector (range cnt))]
          (if (zero? i)
            (map #(map v %) res)
            (recur (unchecked-dec i)
                   (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt)))
                           res))))))))



;; explicitly integrated
(defn combinations9 [coll n]
  (let [choose-indices (fn [cnt choose]
                        ;; {:pre [(pos-int? choose) (<= choose cnt)]}
                        (loop [i (unchecked-dec choose) res (map vector (range cnt))]
                          (if (zero? i)
                            res
                            (recur (unchecked-dec i)
                                   (mapcat (fn [prev] (map #(conj prev %)
                                                           (range (inc (peek prev)) cnt)))
                                           res)))))]
    (if-not (pos-int? n)
      (list ())
      (let [v (vec coll)
            cnt (count v)]
        (when (<= n cnt)
          (map #(map v %) (choose-indices cnt n)))))))



(defn combinations [coll n]
  (let [choose-indices (fn [cnt choose]
                        ;; {:pre [(pos-int? choose) (<= choose cnt)]}
                        (loop [i (dec choose) res (map vector (range cnt))]
                          (if (zero? i)
                            res
                            (recur (dec i)
                                   (mapcat (fn [prev] (map #(conj prev %)
                                                           (range (inc (peek prev)) cnt)))
                                           res)))))]
    (if-not (pos-int? n)
      (list ())
      (let [v (vec coll)
            cnt (count v)]
        (when (<= n cnt)
          (map #(map v %) (choose-indices cnt n)))))))



(defn combinations-sets [coll n]
  (map set (combinations coll n)))




(defn set= [a b] (= (set a) (set b)))

;; peek and inc to n add



(defn smoke-combo [cf]
  (assert (=  (set nil) (set (cf [] 11))))
  (assert (= (set (map set (cf #{:rose :lily :daisy :tulip} 3)))
             (hash-set #{:rose :lily :daisy}, #{:rose :lily :tulip}, #{:rose :daisy :tulip}, #{:lily :daisy :tulip})))
  (dotimes [n 5]
    (let [xs (range n)]
      (dotimes [m n]
        (assert (= (set (map set (mc/combinations xs m))) (set (map set (cf xs m))))
                (str "Testing " m " with " n " items")))))
  true)


