;; https://projecteuler.net/problem=35

;; The number, 197, is called a circular prime because all rotations of the digits: 197,
;; 971, and 719, are themselves prime. There are thirteen such primes below 100: 2, 3, 5, 7,
;; 11, 13, 17, 31, 37, 71, 73, 79, and 97. How many circular primes are there below one
;; million?


(ns miner.euler35)

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

(defn prime-set-below [high]
  (into #{} (take-while #(< % high) lazy-infinite-primes)))

(defn rotations [n]
  (let [digs (mapv #(- (long %) (long \0)) (seq (pr-str n)))
        cnt (count digs)
        ddv (into digs digs)]
    (map (fn [i] (reduce (fn [sum j] (+ (ddv j) (* 10 sum))) 0 (range i (+ i cnt))))
         (range cnt))))

(defn circular-primes [max]
  (let [pset (prime-set-below max)
        circ? (fn [n] (every? pset (rotations n)))]
    (filter circ? pset)))

(defn euler35 []
  (count (circular-primes (long 1e6))))

;; 55




