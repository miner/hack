;;; https://projecteuler.net/problem=21
;;;
;;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide
;;; evenly into n).  If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable
;;; pair and each of a and b are called amicable numbers.  For example, the proper divisors
;;; of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The
;;; proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.  Evaluate the sum of
;;; all the amicable numbers under 10000.

(ns miner.euler21)

(defn zmod? [n d]
  (zero? (rem n d)))

(defn divisors [n]
  (cons 1 (filter #(zmod? n %) (range 2 (inc (quot n 2))))))

(defn dsum [n]
  (reduce + 0 (divisors n)))

(defn amicables [coll]
  (filter (fn [i] (let [d (dsum i)] (and (not= i d) (= (dsum d) i)))) coll))

(defn euler21 []
  (reduce + 0 (amicables (range 1 10001))))

;; 31626



;; caching wasn't faster

(defn e21 []
  (let [coll (range 1 10001)
        cache (reduce (fn [m i] (assoc m i (dsum i))) {} coll)]
    (reduce-kv (fn [sum k v]
                 (if (and (not= k v) (= (cache v) k))
                   (+ sum k)
                   sum))
               0
               cache)))



(defn eu21 []
  (let [coll (range 1 10001)
        cache (reduce (fn [m i] (assoc m i (dsum i))) {} coll)]
    (reduce (fn [sum k]
                 (if (and (not= k (cache k)) (= k (cache (cache k))))
                   (+ sum k)
                   sum))
               0
               coll)))

