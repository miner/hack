(ns miner.sieve
  (:require [clojure.data.int-map :as im]
            [clojure.core.rrb-vector :as fv]
            [clojure.data.avl :as avl]))


;; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

;;; SEM: use rem instead of mod -- it's a bit faster
;;; obvious from looking at source

;; convert infinite, lazy no-arg (nullary) to limited one-arg (unary)
(defn twf [infinite-fn]
  (fn [limit]
    (take-while #(< % limit) (infinite-fn))))




;;; mailing list discussion:
;; http://groups.google.com/group/clojure/browse_thread/thread/735a485e385c6eb7/0a5c1ac268980c6d?#0a5c1ac268980c6d

;; See also cgrand ideas:
;; http://clj-me.cgrand.net/2009/07/30/everybody-loves-the-sieve-of-eratosthenes/

;; I suspect that cgrand's algorithm is really Disjkstra's Primes, not the Sieve of E

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



(defn cg-primes-standard
  ([]  (letfn [(enqueue! [sieve n step]
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
  ([limit]
   (take-while #(< % limit) (cg-primes-standard))))


;; slightly faster with int-map
(defn cg-primes
  ([]  (letfn [(enqueue! [sieve n step]
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
         
         (cons 2 (lazy-seq (next-primes (transient (im/int-map)) 3)))))
  
  ([limit]   (take-while #(< % limit) (cg-primes))))


;; faster if you skip dissoc! but probably uses more memory
(defn cg-primes2
  ([]  (letfn [(enqueue! [sieve n step]
                 (let [m (+ n step)]
                   (if (sieve m)
                     (recur sieve m step)
                     (assoc! sieve m step))))
               
               (next-sieve! [sieve candidate]
                 (if-let [step (sieve candidate)]
                   (-> sieve
                       ;; (dissoc! candidate)
                       (enqueue! candidate step))
                   (enqueue! sieve candidate (+ candidate candidate))))
               
               (next-primes [sieve candidate]
                 (if (sieve candidate)
                   (recur (next-sieve! sieve candidate) (+ candidate 2))
                   (cons candidate
                         (lazy-seq (next-primes (next-sieve! sieve candidate)
                                                (+ candidate 2))))))]
         
         (cons 2 (lazy-seq (next-primes (transient (im/int-map)) 3)))))
  
  ([limit]   (take-while #(< % limit) (cg-primes2))))

;; don't dissoc! for performance
;; use int-map for performance
;; limit=nil for infinite, othewise max candidate to consider
(defn cg-primes3
  ([] (cg-primes3 nil))
  ([limit]
   (letfn [(enqueue! [sieve n step]
             (let [m (+ n step)]
               (if (sieve m)
                 (recur sieve m step)
                 (assoc! sieve m step))))
           
           (next-sieve! [sieve candidate]
             (if-let [step (sieve candidate)]
               (-> sieve
                   ;; (dissoc! candidate)
                   (enqueue! candidate step))
               (enqueue! sieve candidate (+ candidate candidate))))
           
           (next-primes [sieve candidate]
             (when (or (nil? limit) (< candidate limit))
               (if (sieve candidate)
                 (recur (next-sieve! sieve candidate) (+ candidate 2))
                 (cons candidate
                       (lazy-seq (next-primes (next-sieve! sieve candidate)
                                              (+ candidate 2)))))))]
     
     (cons 2 (lazy-seq (next-primes (transient (im/int-map)) 3))))))
  




;; don't call dissoc!
;; simpler and faster but doesn't clear old location so probably uses more memory

;; [org.clojure/data.int-map "0.2.4"]
;; (require '[clojure.data.int-map :as im])

(defn oprimes
  ([] (oprimes nil))

  ([limit]
   (let [update-sieve! (fn [sieve c step]
                         (let [c2 (+ c step)]
                           (if (sieve c2)
                             (recur sieve c2 step)
                             (assoc! sieve c2 step))))

         next-primes (fn next-primes [sieve candidate]
                       (when (or (nil? limit) (< candidate limit))
                         (if-let [step (sieve candidate)]
                           (recur (update-sieve! sieve candidate step) (+ candidate 2))
                           (cons candidate
                                 (lazy-seq (next-primes
                                            (assoc! sieve (* candidate candidate) (* 2 candidate))
                                            (+ candidate 2))))))) ]
     
     (cons 2 (lazy-seq (next-primes (transient (im/int-map)) 3))))))

;;; skipping dissoc! for performance.
;;; potential for saving memory, but it's slower...
;;; (recur (dissoc! (update-sieve! sieve candidate step) candidate) (+ candidate 2))






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
  ;; slow but amazing that it works at all
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


;; See also dijkstra_primes.clj for another algorithm




;; ordered-set is too slow, better to sort at end
;; but even with transient set the sort is too slow

;; Translated from pseudocode on Wikipedia
(defn classic-sieve
  "Returns sequence of primes less than N"
  [n]
  (loop [nums (transient (vec (range n))) i 2]
    (cond
     (> (* i i) n) (remove nil? (nnext (persistent! nums)))
     (nums i) (recur (loop [nums nums j (* i i)]
                       (if (< j n)
                         (recur (assoc! nums j nil) (+ j i))
                         nums))
                     (inc i))
     :else (recur nums (inc i)))))


;; about the same speed so not worth it to use sqrt
(defn cs7
  "Returns sequence of primes less than N"
  [n]
  (let [nsqrt (long (inc (Math/sqrt n)))]
  (loop [nums (transient (vec (range n))) i 2]
    (cond
     (> i nsqrt) (remove nil? (nnext (persistent! nums)))
     (nums i) (recur (loop [nums nums j (* i i)]
                       (if (< j n)
                         (recur (assoc! nums j nil) (+ j i))
                         nums))
                     (inc i))
     :else (recur nums (inc i))))))




;;; SEM idea: do just odd primes
;;; odd-prime_i = 2i+1
;;; step by 2p to skip missing evens

(defn csodd
  "Returns sequence of primes less than N"
  [n]
  (let [nsqrt2 (inc (long (/ (Math/sqrt n) 2.0)))]
    (loop [nums (transient (into [2] (range 3 n 2))) i 1]
      (cond
       (> i nsqrt2) (remove nil? (persistent! nums))
       (nums i) (recur (let [step (* 2 (nums i))]
                         (loop [nums nums j (* (nums i) (nums i))]
                           (if (< j n)
                             (recur (assoc! nums (quot j 2) nil) (+ j step))
                             nums)))
                       (inc i))
       :else (recur nums (inc i))))))



(defn cso4
  "Returns sequence of primes less than N"
  [n]
  (let [nsqrt2 (inc (long (/ (Math/sqrt (double n)) 2.0)))]
    (loop [nums (transient (into [2] (range 3 n 2))) i 1]
      (cond
       (> i nsqrt2) (remove nil? (persistent! nums))
       (nums i) (recur (let [step (* 2 (nums i))]
                         (loop [nums nums j (* (nums i) (nums i))]
                           (if (< j n)
                             (recur (assoc! nums (quot j 2) nil) (+ j step))
                             nums)))
                       (inc i))
       :else (recur nums (inc i))))))

;; quot 2 slightly faster than bitshift






;; Idea about packing odds, sounds like Sundaram
;; https://codereview.stackexchange.com/questions/28902/clojure-code-for-finding-prime-numbers

;; Use packed array of odds, where index i represents value 2i+1. Then you don't have to
;; deal with evens, which are all non-prime a priori (except the 2 of course). Then you can
;; increment by 2*p for a prime p to find its odd multiples twice faster.
;;
;; For a non-marked index i, the prime p is p = 2*i+1, its square is p*p = (2i+1)(2i+1) =
;; 4i^2 + 4i + 1 and its index is (p*p-1)/2 = 2i^2 + 2i = 2i(i+1) = (p-1)(i+1). For the value
;; increment of 2*p, the index increment on 2x-packed array is di = p = 2i+1.

;;; More ideas:
;; https://rosettacode.org/wiki/Extensible_prime_generator#Clojure
;; https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Unbounded_Versions

;; Good paper with Haskell examples:
;; https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;; "The Genuine Sieve of Eratosthenes"
;; Melissa E. O’Neill
;; Harvey Mudd College


;; SEM -- total disaster that hidden def looks like a cache -- no fair and bad style.

;; orig from Rosetta code based on O'Neill paper
(defn ros-primes-hashmap
  "Infinite sequence of primes using an incremental Sieve or Eratosthenes with a Hashmap"
  []
  (letfn [(nxtoddprm [c q bsprms cmpsts]
            (if (>= c q) ;; only ever equal
              (let [p2 (* (first bsprms) 2), nbps (next bsprms), nbp (first nbps)]
                (recur (+ c 2) (* nbp nbp) nbps (assoc cmpsts (+ q p2) p2)))
              (if (contains? cmpsts c)
                (recur (+ c 2) q bsprms
                       (let [adv (cmpsts c), ncmps (dissoc cmpsts c)]
                         (assoc ncmps
                                (loop [try (+ c adv)] ;; ensure map entry is unique
                                  (if (contains? ncmps try)
                                    (recur (+ try adv)) try)) adv)))
                (cons c (lazy-seq (nxtoddprm (+ c 2) q bsprms cmpsts))))))]
    (do (def baseoddprms (cons 3 (lazy-seq (nxtoddprm 5 9 baseoddprms {}))))
        (cons 2 (lazy-seq (nxtoddprm 3 9 baseoddprms {}))))))


;; SEM -- buggy to use def, but I accidentally depended on having that def when I thought I
;; was accessing a local recursively.


;; hacking by SEM
;; TBA





;; for benchmarking inf-primes-fn
(defn p10k [f] (first (drop 10000 (f))))


;;; Misguided stuff trying to estimate number of primes
;; https://en.wikipedia.org/wiki/Prime_number_theorem

;; (defn overestimated-prime-count [x]
;;   (if (< x 10)
;;     10
;;     (let [x (double x)]
;;       (+ 150 (long (/ x (Math/log x)))))))
;; 
;; 
;; (defn estimated-prime-count [x]
;;   (if (< x 10)
;;     10
;;     (let [x (double x)]
;;       (inc (long (/ x (Math/log x)))))))
;; 
;; 

;; by experiment to get 1.3 factor for safety
(defn est-prime-count [x]
  (if (< x 2)
    1
    (let [x (double x)]  (long (* 1.3 (/ x (Math/log x)))))))

;; hack, 1.3 factor is an empirical kludge derived from tests with n < 1e6
(defn limit-for-count [n]
  (if (< n 10)
    30
    (let [n (double n)]
      (long (* 1.0 n (Math/log n))))))
;; maybe 1.2 factor is OK


(defn pcounts [limit]
  (let [pc (reduce-kv (fn [r i p] (into r (repeat (- p (count r)) i)))
                      []
                      (vec (classic-sieve limit)))]
    pc
#_    (reduce min Long/MAX_VALUE (map - (map est-prime-count (range limit)) pc))))

(defn sieve-cnt [cnt]
  (take cnt (classic-sieve (limit-for-count cnt))))

(defn scnt [cnt]
  (let [limit (* 2 (limit-for-count cnt))
        pc (reduce-kv (fn [r i p] (into r (repeat (- p (count r)) i)))
                      []
                      (vec (classic-sieve limit)))
        errors (map - (map limit-for-count (range limit)) pc)]
       
    [(reduce min Long/MAX_VALUE errors)
     (reduce max Long/MIN_VALUE errors)]))




;; slow compared to classic-sieve
;; need to adjust from limit down, as algo goes to 2n + 2

;; https://en.wikipedia.org/wiki/Sieve_of_Sundaram

;; NOTE: my first attempt used :when but it's much better to use :while in the for
;; comprehension.

;; much faster with transients
;; and simplified with more inlined calcs
;; simplified limit to n  to avoid inc/dec
;; transducer style looks a little nicer, but not much performance improvement over obvious

;; still twice as slow as classic-sieve but at least it's competitive with some others
(defn sundaram [limit]
  (let [n (quot (inc limit) 2) ]
    (into [2]
          (comp (remove nil?) (map #(inc (* 2 %))))
          (rest (persistent! (reduce (fn [res x] (assoc! res x nil))
                                     (transient (vec (range n)))
                                     (for [j (range 1 n)
                                           i (range 1 (inc j))
                                           :let [c (+ i j (* 2 i j))]
                                           :while (< c n)]
                                       c)))))))




;; highest prime less than 100000
(def phk 99991)

;; for Eric Normand PurelyFunctional.tv newsletter
(defn en-test [f]
  (let [result (f 100000)]
    (assert (every? true? (map = primes-300 result)))
    (assert (= (last result) 99991))
    (assert (= (reduce + 0 result) 454396537)))
  99991)

(defn smoke-test
  ([f] (smoke-test f 100))
  ([f n] (smoke-test f n nil))
  ([f n coll]
   (let [res (or coll (f n))]
   (assert (every? true? (map = res primes-300)))
   (last res))))



(require '[criterium.core :as c])

(defn ben [& fs]
  (doseq [f fs]
    (println)
    (println (str f))
    (c/quick-bench (en-test f))))





