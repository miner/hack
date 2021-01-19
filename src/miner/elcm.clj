(ns miner.elcm)

;; https://gist.github.com/ericnormand/250e0ab50d6e0d4f5a7db75e2dd86260

;; rem is slightly faster than mod, but you have to guarantee all pos ints.
;; packaged as single function
(defn lcm-sem [xs]
  #_ {:pre [(every? pos? xs)]}
  (let [gcd (fn [a b]
              (if (zero? b)
                a
                (recur b (rem a b))))
        lcm2 (fn lcm2
               ([] 1)
               ([a] a)
               ([a b] (quot (* a b) (gcd a b)))
               ([a b & more] (reduce lcm2 (lcm2 a b) more)))]
    (apply lcm2 xs)))



;;; steffan-westcott had a good solution.  Slightly revised by SEM.

(defn lcm-sw [nums]
  #_ {:pre [(every? pos-int? nums)]}
  (let [gcd (fn [x y]
              (if (zero? y)
                x
                (recur y (mod x y))))]
    (reduce (fn [x y] (quot (* x y) (gcd x y))) 1 nums)))


;;; More hacked by SEM to use rem (safe and faster for all pos).
;;; This is now my favorite, better than mine.  Assume all pos ints.
(defn lcm [nums]
  #_ {:pre [(every? pos-int? nums)]}
  (let [gcd (fn [x y]
              (if (zero? y)
                x
                (recur y (rem x y))))]
    (reduce (fn [x y] (quot (* x y) (gcd x y))) 1 nums)))


(defn smoke-lcm
  ([] (smoke-lcm lcm))
  ([lcm]
   (assert (= (lcm [1 2 3]) 6))
   (assert (= (lcm [5 4 4]) 20))
   (assert (= (lcm []) 1))
   (assert (= (lcm [10]) 10))
   (assert (= (lcm [Integer/MAX_VALUE 557 33]) 39472896915507))
   true))



;; https://en.wikipedia.org/wiki/Binary_GCD_algorithm

;; Fun to try, but for lcm we need mult and div so it's not really practical

;; not quite as fast as regular gcd
(defn bgcd [a b]
  (let [half (fn ^long [^long n] (unsigned-bit-shift-right n 1))]
    (loop [a a b b p 0]
      (cond (= a b) (bit-shift-left a p)
            (zero? a) (bit-shift-left b p)
            (zero? b) (bit-shift-left a p)
            (even? a) (if (odd? b)
                        (recur (half a) b p)
                        (recur (half a) (half b) (inc p)))
            :else (if (even? b)
                    (recur a (half b) p)
                    (if (> a b)
                      (recur (half (- a b)) b p)
                      (recur (half (- b a)) a p)))))))
(defn blcm [nums]
  #_ {:pre [(every? pos-int? nums)]}
  (let [half (fn ^long [^long n] (unsigned-bit-shift-right n 1))
        bgcd (fn [a b]
               (loop [a a b b p 0]
                 (cond (= a b) (bit-shift-left a p)
                       (zero? a) (bit-shift-left b p)
                       (zero? b) (bit-shift-left a p)
                       (even? a) (if (odd? b)
                                   (recur (half a) b p)
                                   (recur (half a) (half b) (inc p)))
                       :else (if (even? b)
                               (recur a (half b) p)
                               (if (> a b)
                                 (recur (half (- a b)) b p)
                                 (recur (half (- b a)) a p))))))]
    (reduce (fn [x y] (quot (* x y) (bgcd x y))) 1 nums)))

