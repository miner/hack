
;;; Neal Ford
;;; http://www.ibm.com/developerworks/library/j-ft17/

;;; 10/16/12  15:57 by miner -- slightly modified
;;; see also sieve.clj for the best way to do primes
;;; see also dijkstra_primes.clj for an interesting algorithm


;;; 01/22/25 10:29 by miner -- see also Hacker News discussion of interesting, palindromic
;;; or memorable primes.  Lots of good ones.  https://news.ycombinator.com/item?id=42748691

(ns miner.primes
  (:require [clojure.math :as m]))

;;; just a few
(def memorable-primes
  [34567876543
   33321112333
   11114741111
   35753335753
   131
   13331
   31337
   8675309 ;; Jenny
   3212123
   18181
   30103
   ;; last two are bigints
   11111111111111111111111N ;; 23 ones
   12345678910987654321N  ;; 1 through 10 and down again
   ])

(def jenny 8675309)

   
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

;;; faster with vectors, inlined factor test, eager with transducer
;;; should be faster if you guarantee odd N and only check for odd factors
;;; would be better to only check prime factors but we're not that smart
(defn factorv [n]
  (when (odd? n)
    (let [factors-below-sqrt (filterv #(zero? (rem n %)) (range 3 (inc (Math/sqrt n)) 2))]
      (into (into factors-below-sqrt [1 n])
            (map #(/ n %))
            factors-below-sqrt))))


(def get-factors (memoize factors))

(defn sum-factors [n]
  (reduce + (get-factors n)))

;; SEM:  use = for all ints.  == only if mixed num types
(defn primef? [n]
  (or (= n 2) (= (inc n) (sum-factors n))))

;;; You don't need to actually calculate all the factors, just check for odds above 2
;;; much faster than primef? but not great, only need to check odd factors
;;; but see prime? below for faster way
(defn primef2? [n]
  (if (even? n)
    (= n 2)
    (and (> n 2) (empty? (filterv #(zero? (rem n %)) (range 3 (inc (Math/sqrt n)) 2))))))

;;; note that sqrt(3) + 1 < 3 so it correctly returns true for 3 but almost by accientd

;lazy is better in general but slower on actual prime
(defn primef3? [n]
  (if (even? n)
    (= n 2)
    (and (> n 2) (not-any? #(zero? (rem n %)) (range 3 (inc (Math/sqrt n)) 2)))))

;; http://sprott.physics.wisc.edu/pickover/pc/1000000000000066600000000000001.html
;; In John Milton's Paradise Lost, Belphegor is one of the "Principalities of the Prime." In hell, Belphegor is the demon of inventiveness.
;; Too big to be used in a practical test, but it would be fun to build it into our prime? predicate.
(def belphegor 1000000000000066600000000000001N)

(defn bprime? [n]
  (or (== n 2) (== n belphegor) (== (inc n) (sum-factors n))))





;;; https://en.wikipedia.org/wiki/AKS_primality_test
;;; SEM: looks too complicated for me to code at the moment.  :-)

;;; AKS is test for primality, but not the fastest for reasonable numbers (apparently).
;;; Rosetta code gives a wrong explanation so that code is not the actual AKS.  It's an old
;;; slow way, that is used to explain AKS, which is much better but more complicated.

;; https://rosettacode.org/wiki/AKS_test_for_primes#Clojure
(defn c 
  "kth coefficient of (x - 1)^n"
  [n k] 
  (/ (apply *' (range n (- n k) -1)) 
     (apply *' (range k 0 -1))
     (if (and (even? k) (< k n)) -1 1)))
 
(defn cs 
  "coefficient series for (x - 1)^n, k=[0..n]"
  [n] 
  (map #(c n %) (range (inc n))))
 
(defn aks? [p] (->> (cs p) rest butlast (every? #(-> % (mod p) zero?))))

(defn demo
  ([] (demo aks?))
  ([aks?]
  (println "coefficient series n (k[0] .. k[n])")
  (doseq [n (range 10)] (println n (cs n)))
  (println)
  (println "primes < 50 per AKS:" (filter aks? (range 2 50)))))


;; SEM refactoring to be faster, but still very slow and not a good idea!
(defn aks2? [p]
  (let [co (fn [n k]
             ;; kth coefficient of (x - 1)^n
             (/ (reduce *' (if (and (even? k) (< k n)) -1 1) (range (inc (- n k)) (inc n)))
                (reduce *' 1 (range 2 (inc k)))))]
    (every? #(zero? (mod (co p %) p))
            (range 1 p))))


(defn smoke-aks [aks?]
  (assert (= (filter aks? (range 2 50)) '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)))
  true)



;;; See Java  BigInteger.isProbablePrime  -- but kind of slow, 3x primef? shown above, which is
;;; not considered fast.

;;; Works well when numbers get big.  If it returns false, the number is definitely
;;; composite.  If it returns true, the number is probably prime but still might be
;;; composite.  Optional second arg is "uncertainty" tolerance: 0 always returns
;;; true (totally unreliable); a more positive unc yields a more reliable true result.
;;; Technically, the probability of a correct true is (1 - 2^-u).  A false result is always
;;; correct.


(defn probable-prime?
  ([n] (probable-prime? n 5))
  ([n unc] (java.math.BigInteger/.isProbablePrime (biginteger n) unc)))

;; fastest for single p test
;; inc-inc slightly faster than +2
;; just repeated div (rem), not sieve
(defn prime? [n]
  (if (even? n)
    (= n 2)
    (and (> n 1)
         (let [sqrt (Math/sqrt n)]
           (loop [candidate 3]
             (cond (> candidate sqrt) true
                   (zero? (rem n candidate)) false
                   :else (recur (inc (inc candidate)))))))))


;; slow
(defn primez?
  ([^long n]
   (if (even? n)
     (= n 2)
     (and (> n 2)
          (reduce (fn [res candidate]
                    (if (zero? (rem n candidate))
                      (reduced false)
                      res))
                    true
                    (range 3 (inc (Math/sqrt n)) 2))))))


;;; very slow!
(defn primena?
  ([^long n]
   (if (even? n)
     (= n 2)
     (and (> n 2)
          (not-any? #(zero? (rem n %)) (range 3 (inc (long (Math/sqrt (double n)))) 2))))))


(def prime-cache-1000
  #{2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109
    113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233
    239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367
    373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499
    503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643
    647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797
    809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947
    953 967 971 977 983 991 997})

;; surprisingly the cache doesn't really improve performance for < 1000
;; maybe need 10K or more to get benefit?
(defn prime1k? [n]
  (if (< n 1000)
    (prime-cache-1000 n)
    (prime? n)))

                

(def odd-prime-cachev
  [3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109
   113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233
   239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337 347 349 353 359 367
   373 379 383 389 397 401 409 419 421 431 433 439 443 449 457 461 463 467 479 487 491 499
   503 509 521 523 541 547 557 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643
   647 653 659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797
   809 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937 941 947
   953 967 971 977 983 991 997])

(def odd-prime-cache-max (peek odd-prime-cachev))

(def odd-prime-cache-max-sq (* odd-prime-cache-max odd-prime-cache-max))

(def odd-prime-cache-set (set odd-prime-cachev))

;;; trying to be clever, but it's still slower than the simple prime?
;;; the type hints did help
(defn cprime? [^long n]
  (cond (even? n) (= n 2)
        (< n 2) false
        (<= n odd-prime-cache-max) (odd-prime-cache-set n)
        :else (let [rt (long (m/ceil (m/sqrt n)))]
                (reduce (fn [res ^long p]
                          (cond (> p rt) (reduced true)
                                (zero? (rem n p)) (reduced false)
                                :else true))
                        true
                        (if (< rt odd-prime-cache-max)
                          odd-prime-cachev
                          (concat odd-prime-cachev
                                  (range (+ odd-prime-cache-max 2) rt 2)))))))


;;; slower with loop, slower than cprime?
(defn lprime? [^long n]
  (cond (even? n) (= n 2)
        (< n 2) false
        (<= n odd-prime-cache-max) (odd-prime-cache-set n)
        :else (let [rt (long (m/ceil (m/sqrt n)))]
                (loop [cs  (if (< rt odd-prime-cache-max)
                             odd-prime-cachev
                             (concat odd-prime-cachev
                                     (range (+ odd-prime-cache-max 2) rt 2)))]
                  (let [p (long (first cs))]
                    (cond (> p rt) true
                          (zero? (rem n p)) false
                          :else (recur (rest cs))))))))

#_  (quick-bench (count (filterv cprime? (range 100000))))


;;; still not faster, cache doesn't seem to help,  regular prime? is still faster on 10K
;;; my guess is too many conditionals slows down the execution
(defn iprime? [^long n]
  (cond (even? n) (= n 2)
        (< n 2) false
        (<= n odd-prime-cache-max) (odd-prime-cache-set n)
        :else (let [rt (long (m/ceil (m/sqrt n)))]
                (loop [i 0]
                  (if (< i (count odd-prime-cachev))
                    (let [p (odd-prime-cachev i)]
                      (cond (> p rt) true
                            (zero? (rem n p)) false
                            :else (recur (inc i))))
                    (loop [p (+ odd-prime-cache-max 2)]
                      (cond (> p rt) true
                            (zero? (rem n p)) false
                            :else (recur (inc (inc p))))))))))
