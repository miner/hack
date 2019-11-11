
;;; Neal Ford
;;; http://www.ibm.com/developerworks/library/j-ft17/

;;; 10/16/12  15:57 by miner -- slightly modified
;;; see also sieve.clj for the best way to do primes
;;; see also dijkstra_primes.clj for an interesting algorithm

(ns miner.primes)

(defn factor? [n potential]
  (zero? (rem n potential)))

;; bad: using flatten
(defn ford-factors [n]
  (let [factors-below-sqrt (filter #(factor? n %) (range 1 (inc (Math/sqrt n))))
        factors-above-sqrt (map #(/ n %) factors-below-sqrt)]
    (flatten (conj factors-below-sqrt factors-above-sqrt))))

;; SEM: better with concat
(defn factors [n]
  (let [factors-below-sqrt (filter #(factor? n %) (range 1 (inc (Math/sqrt n))))
        factors-above-sqrt (map #(/ n %) factors-below-sqrt)]
    (concat factors-below-sqrt factors-above-sqrt)))

(def get-factors (memoize factors))

(defn sum-factors [n]
  (reduce + (get-factors n)))

;; SEM:  use = for all ints.  == only if mixed num types
(defn prime? [n]
  (or (= n 2) (= (inc n) (sum-factors n))))


;; http://sprott.physics.wisc.edu/pickover/pc/1000000000000066600000000000001.html
;; In John Milton's Paradise Lost, Belphegor is one of the "Principalities of the Prime." In hell, Belphegor is the demon of inventiveness.
;; Too big to be used in a practical test, but it would be fun to build it into our prime? predicate.
(def belphegor 1000000000000066600000000000001N)

(defn bprime? [n]
  (or (== n 2) (== n belphegor) (== (inc n) (sum-factors n))))

