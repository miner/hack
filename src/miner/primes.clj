
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
(defn primef? [n]
  (or (= n 2) (= (inc n) (sum-factors n))))


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

(defn probable-prime? [n]
  (.isProbablePrime (java.math.BigInteger/valueOf ^long n) 5))


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

                
  
