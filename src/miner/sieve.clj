(ns miner.sieve)

;;; SEM: use rem instead of mod -- it's a bit faster
;;; obvious from looking at source


;;; mailing list discussion:
;;http://groups.google.com/group/clojure/browse_thread/thread/735a485e385c6eb7/0a5c1ac268980c6d?#0a5c1ac268980c6d

;; See also cgrand ideas:
;; http://clj-me.cgrand.net/2009/07/30/everybody-loves-the-sieve-of-eratosthenes/

(defn lazy-primes3 []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                (lazy-seq (next-primes (next-sieve sieve candidate)
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))



;; SEM: even faster with transients
(defn trans-primes3 []
  (letfn [(enqueue! [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc! sieve m step))))
          (next-sieve! [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc! candidate)
                (enqueue! candidate step))
              (enqueue! sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve! sieve candidate) (+ candidate 2))
              (cons candidate
                (lazy-seq (next-primes (next-sieve! sieve candidate)
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes (transient {}) 3)))))

;; Of course, in a real app, you'd probably just pre-compute a limited set of primes that
;; you might need.  The lazy-infinite-primes (as a lazy seq, not function) is a good
;; compromise if you never need to recover the memory from a long list.


;;; Tried using contrib data.int-map for slightly faster map.  Not faster than the standard
;;; trans.  Interestingly, the im/int-set is faster than its trans version, but about the
;;; same as the regular trans.





;; --

;; Look at contrib lazy_seqs.clj primes for fastest known way (a lazy sequence, not a function).
;; This is my refactored version.  There is still an issue that you use as much memory as the
;; longest list of primes that you ever ask for.

(def lazy-infinite-primes (concat [2 3 5 7]
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

(defn primes [nprimes]
  (take nprimes lazy-infinite-primes))

;; -----

;; http://clojure.lang.dk/?p=101

(defn sieve [xs]
  (filter #(not (zero? (mod % (first xs)))) (rest xs)))

(defn rsieve [xs]
  (map first (iterate sieve xs)))

(defn primes-below [max]
  (take-while #(not (nil? %))
	      (rsieve (range 2 max))))



;; https://clojuredocs.org/clojure.core/lazy-seq
;; I renamed lazy-sieve so I could hack on it
(defn lazy-sieve [s]
  (cons (first s)
        (lazy-seq (lazy-sieve (filter #(not= 0 (rem % (first s)))
                                 (rest s))))))

(take 20 (lazy-sieve (iterate inc 2)))
;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71)


;; SEM: my version based on above
(defn infinite-primes []
  (let [sieve (fn sieve [s]
                ;; first item is always a known prime, rest are candidates
                (lazy-seq
                 (cons (first s)
                       (sieve (remove #(zero? (rem % (first s))) (rest s))))))]
    (sieve (cons 2 (iterate #(+ % 2) 3)))))

;; could cheat by precomputing more than just 2, but not really worth it

;; Or break it out
(defn lazy-sieve [s]
  ;; first item is always a known prime, rest are candidates
  (lazy-seq (cons (first s) (lazy-sieve (remove #(zero? (rem % (first s))) (rest s))))))

(defn lazy-primes []
  (lazy-sieve (cons 2 (iterate #(+ % 2) 3))))


;;; -----
;;; SEM

(defn take-until-nil [coll]
  (take-while (complement nil?) coll))

(defn primes-bel [max]
  (cons 2 (take-until-nil (map first (iterate sieve (range 3 max 2))))))


;; SEM: but of course, you can't just go dropping over and over as many drops will overlap
;; SEM experimenting with a drop versus a mod
(defn drop-nth
  "Returns a lazy seq by dropping every nth item in coll."
  [n coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (concat (take (dec n) s) (drop-nth n (drop n s))))))

(defn drop-nth2
  "Returns a lazy seq by dropping every nth item in coll."
  [n coll]
  (letfn [(stepfn [n i xs]
            (lazy-seq
             (when-let [s (seq xs)]
               (if (pos? i)
                 (conj (stepfn n (dec i) (rest s)) (first s))
                 (stepfn n n (rest s))))))]
    (stepfn (dec n) (dec n) coll)))



;; Buggy at end -- shouldn't 
(defn rep-nth
  "Returns a lazy seq by dropping every nth item in coll."
  [n val coll]
    (lazy-seq
     (when-let [s (seq coll)]
       (concat (take (dec n) s) [val] (rep-nth n val (drop n s))))))



;; ---

;;; http://clojuredocs.org/clojure_core/clojure.core/keep

;; Sieve of Eratosthenes by using 'keep'.

(defn keep-mcdr-orig [f coll]
  (lazy-seq
     (when-let [x (first coll)]
       (cons x  (keep-mcdr-orig f (f x (rest coll)))))))

(defn prime-numbers-orig [n]
  (cons 1
    (keep-mcdr-orig
     (fn[x xs] (if (not-empty xs)
             (keep #(if-not (zero? (rem % x)) %)
               xs)))
     (range 2 n))))



(defn keep-mcdr [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [p (first s)]
       (cons p (keep-mcdr f (f p (rest s))))))))

(defn primes-keep-below [n]
  (keep-mcdr
   (fn [p xs]
     (when-let [s (seq xs)]
       (remove #(zero? (rem % p)) s)))
   (range 2 n)))



;; Crazy way to test primes with a regex
;; http://neilk.net/blog/2000/06/01/abigails-regex-to-test-for-prime-numbers/

;; perl -wle 'print "Prime" if (1 x shift) !~ /^1?$|^(11+?)\1+$/' [number]


(defn regex-prime? [n]
  ;; slow but wonderful
  (not (re-matches #"1?|(11+?)\1+" (apply str (repeat n "1")))))

(defn regex-primes []
  (filter regex-prime? (cons 2 (iterate #(+ % 2) 3))))


;; http://en.wikipedia.org/wiki/Palindromic_prime

(def palindromic-primes-42  [2,3,5,7,11,101,131,151,181,191,313,353,373,383,
                             727,757,787,797,919,929,10301,10501,10601,11311,
                             11411,12421,12721,12821,13331,13831,13931,14341,
                             14741,15451,15551,16061,16361,16561,16661,17471,
                             17971,18181])

(assert (= (count palindromic-primes-42) 42))

;; SEM might be interesting to generate these

;; Stuff about lucky numbers moved to the lucky project

  
  



(def primes-300 [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
                 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197
                 199 211 223 227 229 233 239 241 251 257 263 269 271 277 281 283 293 307 311
                 313 317 331 337 347 349 353 359 367 373 379 383 389 397 401 409 419 421 431
                 433 439 443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557
                 563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653 659 661
                 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769 773 787 797 809
                 811 821 823 827 829 839 853 857 859 863 877 881 883 887 907 911 919 929 937
                 941 947 953 967 971 977 983 991 997 1009 1013 1019 1021 1031 1033 1039 1049
                 1051 1061 1063 1069 1087 1091 1093 1097 1103 1109 1117 1123 1129 1151 1153
                 1163 1171 1181 1187 1193 1201 1213 1217 1223 1229 1231 1237 1249 1259 1277
                 1279 1283 1289 1291 1297 1301 1303 1307 1319 1321 1327 1361 1367 1373 1381
                 1399 1409 1423 1427 1429 1433 1439 1447 1451 1453 1459 1471 1481 1483 1487
                 1489 1493 1499 1511 1523 1531 1543 1549 1553 1559 1567 1571 1579 1583 1597
                 1601 1607 1609 1613 1619 1621 1627 1637 1657 1663 1667 1669 1693 1697 1699
                 1709 1721 1723 1733 1741 1747 1753 1759 1777 1783 1787 1789 1801 1811 1823
                 1831 1847 1861 1867 1871 1873 1877 1879 1889 1901 1907 1913 1931 1933 1949
                 1951 1973 1979 1987])

(assert (= (count primes-300) 300))


(defn palindromic? [n]
  (let [s (str n)]
    (= s (.toString (.reverse (StringBuilder. s))))))
