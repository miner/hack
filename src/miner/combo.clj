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






;; NOTE:  Eric wanted sets returned so you need one more level of (set ...)



;; SEM: idea -- think of mask walking across vector
;; generate the masks then convolve them
;; For n=3, three in a row, one-skip-three-four, one-skip-skip-four-five, 
;; NOT IMPLEMENTED

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



;; I prefer simple collections in my results because they're faster.  But Eric Normand
;; wanted sets, which is logically correct so I did a version for him below.
(defn combinations-SEM [coll n]
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


;; submitted to Eric.  Don't change now.
;; Eric used sets which is slower, but logical.
;; (into #{} xform ...) seems pretty fast
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
          (map #(into #{} (map v) %) (choose-indices cnt n)))))))


(defn combinations-sets2 [coll n]
  (let [choose-indices (fn [cnt choose]
                        {:pre [(pos-int? choose) (<= choose cnt)]}
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
          (map #(into #{} (map v) %) (choose-indices cnt n)))))))



;; (into #{}...) is faster than (set (map...))



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





;; BEST SO FAR -- icombos is much faster
(defn sem-choose [cnt choose]
 ;; {:pre [(pos-int? choose) (<= choose cnt)]}
 (loop [i (dec choose) res (mapv vector (range cnt))]
   (if (zero? i)
     res
     (recur (dec i)
            (into []
                  (mapcat (fn [prev]
                            (eduction (map #(conj prev %)) (range (inc (peek prev)) cnt))))
                  res)))))


;; nesting eduction is not faster, probably too much rework as eduction is not cached

;; maybe faster but maybe not
(defn sem-choose-ix [cnt choose]
  ;; {:pre [(pos-int? choose) (<= choose cnt)]}
  (reduce (fn [vv _]
            (into []
                  (mapcat (fn [prev]
                            (map #(conj prev %)
                                 (range (inc (peek prev)) cnt))))
                  vv))
          (mapv vector (range cnt))
          (range (dec choose))))




(defn sem-choose-WAS [cnt choose]
 ;; {:pre [(pos-int? choose) (<= choose cnt)]}
 (loop [i (unchecked-dec choose) res (map vector (range cnt))]
   (if (zero? i)
     res
     (recur (unchecked-dec i)
            (mapcat (fn [prev] (map #(conj prev %)
                                    (range (unchecked-inc (peek prev)) cnt)))
                    res)))))



;; faster with transducers
(defn sem-choose1 [cnt choose]
 ;; {:pre [(pos-int? choose) (<= choose cnt)]}
 (loop [i (dec choose) res (map vector (range cnt))]
   (if (zero? i)
     res
     (recur (dec i)
            (into []
                  (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))))
                  res)))))

;; best so far, nested transducers
(defn sem-choose2 [cnt choose]
 ;; {:pre [(pos-int? choose) (<= choose cnt)]}
 (loop [i (dec choose) res (map vector (range cnt))]
   (if (zero? i)
     res
     (recur (dec i)
            (into []
                  (mapcat (fn [prev]
                            (into []
                                  (map #(conj prev %))
                                  (range (inc (peek prev)) cnt))))
                  res)))))

;; how can I pipeline the nested transducers?


;; unchecked is only slightly faster
(defn sem-choose2 [cnt choose]
 ;; {:pre [(pos-int? choose) (<= choose cnt)]}
 (loop [i (unchecked-dec choose) res (map vector (range cnt))]
   (if (zero? i)
     res
     (recur (unchecked-dec i)
            (into [] (mapcat (fn [prev] (map #(conj prev %)
                                    (range (unchecked-inc (peek prev)) cnt))))
                    res)))))



;; Rosetta code
;; https://rosettacode.org/wiki/Combinations#Clojure

#_
(defn combinations
  "If m=1, generate a nested list of numbers [0,n)
   If m>1, for each x in [0,n), and for each list in the recursion on [x+1,n), cons the two"
  [m n]
  (letfn [(comb-aux
	   [m start]
	   (if (= 1 m)
	     (for [x (range start n)]
	       (list x))
	     (for [x (range start n)
		   xs (comb-aux (dec m) (inc x))]
	       (cons x xs))))]
    (comb-aux m 0)))

;; SEM swapped args for consistency with other code
(defn rchoose
  "If m=1, generate a nested list of numbers [0,n)
   If m>1, for each x in [0,n), and for each list in the recursion on [x+1,n), cons the two"
  [n m]
  (letfn [(comb-aux [m start]
            (if (= 1 m)
              (for [x (range start n)] (list x))
              (for [x (range start n)
                    xs (comb-aux (dec m) (inc x))]
                (cons x xs))))]
    (comb-aux m 0)))

;; slower
(defn rchoose2
  "If m=1, generate a nested list of numbers [0,n)
   If m>1, for each x in [0,n), and for each list in the recursion on [x+1,n), cons the two"
  [n m]
  (letfn [(comb-aux [m start]
            (if (zero? m)
              (list ())
              (for [x (range start n)
                    xs (comb-aux (dec m) (inc x))]
                (cons x xs))))]
    (comb-aux m 0)))



;; SEM failed idea: macro that generates appropriate for loop but won't work for general
;; case where rn and choose are calculated at runtime.  Fail!



(defn icombos [cnt choose]
  #_ {:pre [(pos-int? choose) (>= cnt choose)]}
  (loop [res (map vector (range cnt))]
    (if (= (count (first res)) choose)
      res
      (recur (mapcat (fn [prev] (map #(conj prev %) (range (inc (peek prev)) cnt))) res)))))

;; cannot use transients because we need persistent prev (multiple additions)

(defn fact [n]
  (reduce * (range 2 (inc n))))

;; The formula for number of combinations is generally n! / (r! (n - r)!), where n is the total
;; number of possibilities to start and r is the number of selections made.
(defn count-combos-classic [cnt choose]
  (quot (fact cnt) (* (fact choose) (fact (- cnt choose)))))

;; slightly faster
(defn count-combos [cnt choose]
  (quot (reduce * (range (inc choose) (inc cnt)))
        (reduce * (range 2 (inc (- cnt choose))))))


;;; icombos is faster and prettier
(defn kcombos [cnt choose]
  (let [start (vec (range choose))
        final (vec (range (- cnt choose) cnt))
        kinc (fn [vvv]
               (loop [vvv vvv i (dec choose)]
                 (when-not (neg? i)
                   (if (= (vvv i) (final i))
                     (recur vvv (dec i))
                     (reduce (fn [v j] (assoc v j (inc (v (dec j)))))
                             (update vvv i inc)
                             (range (inc i) choose))))))]
    (into [] (take-while some?) (iterate kinc start))))




;;;;;; JUNK

;; disabled by COMMENT

(comment 

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




;;; END COMMENT
)
