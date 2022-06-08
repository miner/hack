(ns miner.goldbach)

;; https://en.wikipedia.org/wiki/Goldbach%27s_conjecture

;; Goldbach's conjecture is one of the oldest and best-known unsolved problems in number
;; theory and all of mathematics. It states that every even natural number greater than two
;; is the sum of two prime numbers.

;;; borrowed from my sieve.clj
(def lazy-infinite-primes
  (concat [2 3 5 7]
          (lazy-seq
           (let [primes-from
                 (fn primes-from [n [f & r]]
                   (if (some #(zero? (rem n %))
                             (take-while #(<= (* % %) n) lazy-infinite-primes))
                     (recur (+ n f) r)
                     (lazy-seq (cons n (primes-from (+ n f) r)))))
                 wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
                               6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
                               2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
             (primes-from 11 wheel)))))

  
;; inclusive A, exclusive B -- like `range` -- but only primes
(defn prime-range [a b]
  (sequence (comp (drop-while #(< % a)) (take-while #(< % b))) lazy-infinite-primes))
  

(defn goldbach? [n]
  (or (odd? n)
      (<= n 2)
      (first (for [p1 (prime-range 2 n)
                   p2 (prime-range (- n p1) n)
                   :when (= n (+ p1 p2))]
               (list p1 p2)))))


#_
(every? goldbach? (range 100000))
;=> true
