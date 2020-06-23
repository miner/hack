;; https://gist.github.com/ericnormand/3c8d180a91d417ce67868bbbac8fb932

;; iterated square root
;; 
;; If you iteratively apply the square root, you will eventually get a number strictly less
;; than two. Write a function that figures out how many times you have to apply the square root
;; it results in a number less than two.
;; 
;; Examples
;; 
;; (iter-sqrt 1) ;=> 0 (since 1 is already less than 2)
;; (iter-sqrt 2) ;=> 1 (since the square root of 2 is less than 2)
;; (iter-sqrt 256) ;=> 4
;;
;; Bonus: Write it in a way that uses stack frames and in a way that doesn't.

(ns miner.esqrt)

;; by far, fastest
(defn iter-sqrt [n]
  (loop [x (double n) i 0]
    (if (< x 2.0)
      i
      (recur (Math/sqrt x) (inc i)))))

(defn isqrt [n]
  (count (take-while #(>= % 2.0) (iterate (fn [x] (Math/sqrt x)) (double n)))))


(defn tsqrt [n]
  (count (into [] (take-while #(>= % 2.0)) (iterate (fn [x] (Math/sqrt x)) (double n)))))

;; slower
(defn seqsqrt [n]
  (count (sequence (take-while #(>= % 2.0)) (iterate (fn [x] (Math/sqrt x)) (double n)))))


(defn smoke-sqrt
  ([] (smoke-sqrt iter-sqrt))
  ([iter-sqrt]
   (assert (zero? (iter-sqrt 1)))
   (assert (= 1 (iter-sqrt 2)))
   (assert (= 4 (iter-sqrt 256)))
   (assert (= 6 (iter-sqrt Long/MAX_VALUE)))
   true))
