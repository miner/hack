;; http://farenda.com/clojure/gcd-in-clojure-using-euclidean-algorithm/

(defn euclid-for-two
  "Calculate GCD of given two numbers using Euclidean algorithm."
  [a b]
  (if (zero? b)
    (Math/abs a)
    (recur b (rem a b))))

(defn euclid
  "Calculate GCD of given numbers using Euclidean algorithm."
  [a b & others]
  (cond
    (and (zero? b) (nil? others)) (Math/abs a)
    (zero? b) (recur a (first others) (next others))
    :else (recur b (rem a b) others)))


(defn euclid2
  "Calculate GCD of given numbers using Euclidean algorithm."
  [^long a ^long b & others]
  (cond
    (and (zero? b) (nil? others)) (Math/abs a)
    (zero? b) (recur a (first others) (next others))
    :else (recur b (rem a b) others)))

;; SEM: I thought this would be a much cleaner way to write it

(defn euc1
  "Calculate GCD of given two numbers using Euclidean algorithm."
  ([a b]
   (if (zero? b)
	   (Math/abs a)
	   (recur b (rem a b))))
  ([a b & others] (reduce euc1 (euc1 a b) others)))

;; BUT it is slower!  Hmmm, maybe needs ^long???  The recur can use primitives that the
;; reduce can't????


(defn euc
  (^long [^long a ^long b]
   (if (zero? b)
	   (Math/abs a)
	   (recur b (rem a b))))
  ([a b & others] (reduce euc (euc a b) others)))

(defn euc2
  ([^long a ^long b]
   (if (zero? b)
	   (Math/abs a)
	   (recur b (rem a b))))
  ([a b & others] (reduce euc2 (euc2 a b) others)))


(defn smoke-test [euclid]
  (and 
   (= 42 (euclid 0 42))
   (= 120 (euclid 1080 1920))
   (= 120 (euclid 1920 1080))
   (= 3 (euclid 21 -9))
   (= 3 (euclid -21 9))
   (= 3 (euclid -6 12 9))
   (= 1 (euclid 5 8 13))
   (= 120 (euclid 1920 1080 1920 1080 1920 1080 1920 1080 1920 1080 1920 1080 1920 1080))))
