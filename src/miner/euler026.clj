(ns miner.euler026
  (:require [clojure.core.reducers :as r]))


;; Original by mishadoff
;; http://mishadoff.com/blog/clojure-euler-problem-026/
;; https://github.com/mishadoff/project-euler/blob/master/src/project_euler/problem026.clj

;; calculate cycle for 1/denom
;; e.g 1/3 = 0.(3), cycle is 3, length 1
;; e.g 1/7 = 0.(142857), cycle is 142857, length is 6

(defn unit-fraction [denom]
  (loop [numer 1 i 1 known {}]
    (let [r (rem (* 10 numer) denom)]
      (cond (zero? r) 0
            (get known r) (- i (get known r))
            :else (recur r (inc i) (assoc known r i))))))

;; "Elapsed time: 312.829133 msecs"
;; [SEM: "Elapsed time: 29.384183 msecs" on my iMac]
(defn euler-026 []
  (->> (range 1 1000)
       (map #(vec [% (unit-fraction %)]))
       (apply max-key second)
       (first)))


;; SEM my stuff

;; I renamed unit-fraction to period.

;; slightly faster to make a vector for known digits, rather than map
;; much faster to use transient vector
;; transient map doesn't help
;; tried clojure.data.int-map but it didn't help much (transient vector was much faster)
;; tried (vector-of :int ...) but that was slow because couldn't use transient

(defn period2 [denom]
  (loop [numer 1 i 1 known (transient (vec (repeat denom nil)))]
    (let [r (rem (* 10 numer) denom)]
      (cond (zero? r) 0
            (get known r) (- i (get known r))
            :else (recur r (inc i) (assoc! known r i))))))


;; java int-array makes it much faster to bash in place
(defn period-GOOD [denom]
  (let [known (int-array denom)]
    (loop [numer 1 i 1]
      (let [r (rem (* 10 numer) denom)]
        (cond (zero? r) 0
              (zero? (aget known r)) (do (aset known r i) (recur r (inc i)))
              :else (- i (aget known r)))))))

;; Evil twist -- aset returns new val, not array
(defn period [denom]
  (let [remi (int-array denom)]
    (loop [numer 1 i 1]
      (let [r (rem (* 10 numer) denom)]
        (cond (zero? r) 0
              (zero? (aget remi r)) (recur r (inc (aset remi r i)))
              :else (- i (aget remi r)))))))


;; use `vector` instead of `vec [...]`
;; be careful about bounds when denom is 1 or less (I decided to return nil)
;; Not sure about preference for multiple ks giving same length, decided to keep greatest k
;; as the original implementation did.  (That's because max-key keeps the rightmost element
;; on ties.)

(defn e26
  ([] (e26 1000))
  ([n] (when (> n 1)
         (->> (range 1 n)
              (map #(vector % (period %)))
              (apply max-key peek)
              (first)))))


(defn te26
  ([] (te26 1000))
  ([n] (transduce (map (juxt identity period))
                  (fn
                    ([best ku] (if (>= (peek ku) (peek best)) ku best))
                    ([best] (first best)))
                  [nil -1]
                  (range 1 n))))




;; observed but unproven theorem:  (> x (period x)) for all ints
;; confirmed by Wikipedia:   https://en.wikipedia.org/wiki/Repeating_decimal
;; "The period of 1/k for integer k is always ≤ k − 1"
;; SEM: that's the same as "period is always < k"

;; informal proof
;; http://mathforum.org/library/drmath/view/51530.html
;;
;; "If you think about the process of converting the fraction m/n to a decimal, at each
;; stage you divide by n and get a remainder. Since the remainders are all less than n,
;; within n steps you must have the same remainder appearing twice, at which point the
;; sequence of digits begins to repeat."

;; With this observation, it makes sense to start at the high end and cutoff if you've
;; already seen a period greater that the next k (descending values of k can't ever give a
;; higher period).


(defn rf-descending-k
  ([best ku]
   (let [[k u] ku
         ubest (peek best)]
     (cond (>= ubest k) (reduced best)
           (> u ubest) ku
           :else best)))
  ([best] (first best)))

;; By far, the fastest
(defn te26r
  ([] (te26r 1000))
  ([n] (transduce (map (juxt identity period))
                  rf-descending-k
                  [nil -1]
                  (range (dec n) 0 -1))))

;; there's a subtltey to the preference for higher or lower k when period is tied
;; (especially for low n).

(defn dcombine
  ([] [nil -1])
  ([a] a)
  ([a b] (if (>= (peek b) (peek a)) b a))
  ([a b & more] (apply max-key peek (list* a b more))))

;; not any faster (about 9.7 ms)
(defn re26-not-so-fast []
  (first (r/fold  dcombine 
                 (r/map #(vector % (period %)) (range 1 1000)))))


(defn red26
  ([] (red26 1000))
  ([n] (first (r/reduce  dcombine 
                 (r/map #(vector % (period %)) (range 1 n))))))




;; slightly faster 9.5 ms
(defn rr26
  ([] (rr26 1000))
  ([n] (when (> n 1)
         (first (reduce (fn [r d] (if (>= (peek d) (peek r)) d r))
                        [nil -1]
                        (map #(vector % (period %)) (range 1 n)))))))


(defn rr26r
  ([] (rr26r 1000))
  ([n] (when (> n 1)
         (first (reduce rf-descending-k
                        [nil -1]
                        (map #(vector % (period %)) (range (dec n) 0 -1)))))))


;; SEM: If you wanted to do this for large numbers, you might need something like the
;; data.int-map lib.
;; https://github.com/clojure/data.int-map
;; but it wasn't very fast.  int-map is supposed to be good for merging.  Not necessarily
;; updates and gets.
