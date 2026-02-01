(ns miner.prime-factors
  (:require [clojure.math :as m]))

;; http://stackoverflow.com/questions/9556393/clojure-tail-recursion-with-prime-factors

;; amalloy version, short and lazy
;; (I added type hints)
(defn prime-factors
  ([n] (prime-factors n 2))
  ([^long n ^long candidate]
     (cond (<= n 1) ()
           (zero? (rem n candidate)) (cons candidate (lazy-seq (prime-factors (/ n candidate)
                                                                              candidate)))
           :else (recur n (inc candidate)))))

(defn prime-factorsq
  ([n] (prime-factorsq n 2))
  ([n candidate]
     (cond (<= n 1) ()
           (zero? (rem n candidate)) (cons candidate (lazy-seq (prime-factorsq (quot n candidate)
                                                                              candidate)))
           :else (recur n (inc candidate)))))

;; actually fastest if you do something bigger in a tight loop
(defn prime-factors2 [n]
  (letfn [(step [^long n ^long div]
            (when (< 1 n)
              (let [q (quot n div)]
                (cond
                  (< q div)           (cons n nil)
                  (zero? (rem n div)) (cons div (lazy-step q div))
                  :else               (recur n (inc div))))))
          (lazy-step [n div]
            (lazy-seq
              (step n div)))]
    (lazy-step n 2)))


;; My version fixed
(defn prime-factors1
  ([^long n]
   (cond (<= n 1) ()
         (even? n) (cons 2 (lazy-seq (prime-factors1 (/ n 2))))
         :else (prime-factors1 n 3)))
  ([^long n ^long candidate]
   (cond (<= n 1) ()
         (zero? (rem n candidate))
           (cons candidate (lazy-seq (prime-factors1 (/ n candidate) candidate)))
         :else (recur n (+ 2 candidate)))))

;; handle even (2s) first with single arity
;; 2-arity assumes only odd candidates
;; q < cand implies end (no more factors except for original value)
(defn prime-factors3
  ([^long n]
   (cond (<= n 1) ()
         (even? n) (cons 2 (lazy-seq (prime-factors3 (/ n 2))))
         :else (prime-factors3 n 3)))
  ([^long n ^long candidate]
   (when (> n 2)
     (let [q (quot n candidate)]
       (cond (< q candidate) (list n)
             (zero? (rem n candidate)) (cons candidate (lazy-seq (prime-factors3 q candidate)))
             :else (recur n (+ 2 candidate)))))))

(comment
  (dotimes [_ 100]
    (time (reduce + (map (fn [^long n] (reduce + (prime-factors
                                                  (* 128 2 3 5 13 2 2 23 101 n))))
                         (range 100)))))
  )

  
;;; How useful is laziness for factors?  Seems like you will always want all the factors so
;;; you might as well be eager.

(defn prime-factors4
  ([^long n]
   (cond (<= n 1) ()
         (even? n) (cons 2 (prime-factors4 (/ n 2)))
         :else (prime-factors4 n 3)))
  ([^long n ^long candidate]
   (when (> n 2)
     (let [q (quot n candidate)]
       (cond (< q candidate) (list n)
             (zero? (rem n candidate)) (cons candidate (prime-factors4 q candidate))
             :else (recur n (+ 2 candidate)))))))


(defn prime1? [n]
  (= 1 (count (prime-factors4 n))))

;; not faster but tail recursive
(defn prime-factors5
  ([^long n]
   (if (<= n 1) [] (prime-factors5 [] n)))
  ([factors ^long n]  
   (if (even? n) (prime-factors5 (conj factors 2) (/ n 2))
       (prime-factors5 factors n 3)))
  ([factors ^long n ^long candidate]
   (if (> n 2)
     (let [q (quot n candidate)]
       (cond (< q candidate) (conj factors n)
             (zero? (rem n candidate)) (prime-factors5 (conj factors candidate) q candidate)
             :else (recur factors n (+ 2 candidate))))
     factors)))



;;; As found on:
;;; https://clojurecivitas.github.io/math/primes/factorization/sieve_augmented.html

;;; Computes all the prime factors, from 0 up to and including for N.

;;; SEM: quibble on results for 0 and 1 which should not be prime.  Also, it is unconvential
;;; to include the N-th element in the vector result.  Probably, it would be better to give
;;; results from 0..(N-1) (or "half-open") so the result is a vector with N elements.

(defn civitas-prime-factors-vec [n]
  (reduce
   (fn [factors prime]
     (if (= 1 (count (nth factors prime)))
       (reduce
        (fn [factors multiple]
          (let [[composite-divisor :as divisors] (nth factors multiple)]
            (if (< prime composite-divisor)
              (let [remaining-divisor (/ composite-divisor prime)
                    remaining-divisors (nth factors remaining-divisor)
                    prime-divisors (cons prime (rest divisors))]
                (assoc
                 factors multiple
                 (if (< 1 (count remaining-divisors))
                   (concat remaining-divisors prime-divisors)
                   (cons remaining-divisor prime-divisors))))
              factors)))
        factors
        (range (* prime prime) (inc n) prime))
       factors))
   (mapv list (range (inc n)))
   (range 2 (inc (m/sqrt n)))))

;;; fixes 0, 1 to be nil.  Note: still return vector including N-th in last slot.
(defn sem-pf [n]
  (if (< n 2)
    (vec (repeat (inc n) nil))
    (into [nil nil] (drop 2) (civitas-prime-factors-vec n))))



;;; for testing equivalence without caring about internal order of factors
(defn set= [c1 c2]
  (= (map set c1) (map set c2)))


;;; works about same
(defn civ-pf2 [n]
  (if (< n 2)
    (vec (repeat (inc n) nil))
    (reduce
     (fn [factorv prime]
       (if (= 1 (count (nth factorv prime)))
         (reduce
          (fn [factorv multiple]
            (let [[composite-divisor :as divisors] (nth factorv multiple)]
              (if (< prime composite-divisor)
                (let [remaining-divisor (/ composite-divisor prime)
                      remaining-divisors (nth factorv remaining-divisor)
                      prime-divisors (cons prime (rest divisors))]
                  ;;(println " pf2" prime multiple factorv)
                  (assoc factorv multiple
                         (if (< 1 (count remaining-divisors))
                           (concat remaining-divisors prime-divisors)
                           (cons remaining-divisor prime-divisors))))
                factorv)))
          factorv
          (range (* prime prime) (inc n) prime))
         factorv))
     (into [nil nil] (map list) (range 2 (inc n)))
     (range 2 (inc (m/sqrt n))))))




;; faster than generic `into` if you have known vector args
(defmacro intov [v1 v2]
  `(reduce conj ~v1 ~v2))


;;; My new favorite, and fastest.
(defn civ-pf [n]
  (if (< n 2)
    (vec (repeat (inc n) nil))
    (reduce (fn [factorv i]
              (if (= (count (nth factorv i)) 1)
                ;; i is prime
                (reduce (fn [factorv multiple]
                          (let [divisorv (nth factorv multiple)
                                composite-divisor (peek divisorv)]
                            (if (< i composite-divisor)
                              (assoc factorv multiple
                                     (intov (conj (pop divisorv) i)
                                            (nth factorv (quot composite-divisor i))))
                              factorv)))
                        factorv
                        (range (* i i) (inc n) i))
                factorv))
            (into [nil nil] (map vector) (range 2 (inc n)))
            (range 2 (long (inc (m/sqrt n)))))))



;;; transient is slower for main factorv so not worth the trouble.
