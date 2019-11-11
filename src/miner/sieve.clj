(ns miner.sieve
  (:require [clojure.data.int-map :as im]))

;;            [clojure.core.rrb-vector :as fv]
;;            [clojure.data.avl :as avl]


;; https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

;;; More ideas:
;; https://rosettacode.org/wiki/Extensible_prime_generator#Clojure
;; https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Unbounded_Versions
;;  Some bad stuff in those examples.  Nested def???

;; Good paper with Haskell examples:
;; https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;; "The Genuine Sieve of Eratosthenes"
;; Melissa E. O’Neill
;; Harvey Mudd College

;; Note anything that does a bunch of divisions is not the genuine Sieve of E.  That's
;; called trial division.  Read the paper for details.


;;; SEM: use rem instead of mod -- it's a bit faster
;;; obvious from looking at source


;; convert infinite, lazy no-arg (nullary) to limited one-arg (unary)
(defn twf [infinite-fn]
  (fn [limit]
    (take-while #(< % limit) (infinite-fn))))


;;; mailing list discussion:
;; http://groups.google.com/group/clojure/browse_thread/thread/735a485e385c6eb7/0a5c1ac268980c6d?#0a5c1ac268980c6d

;; See also Christophe Grand ideas:
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
;; slightly faster with int-map

;; faster if you skip dissoc! but probably uses more memory
;; This is actually faster than oprimes but I don't know why?
(defn cg-primes
  ([]  (letfn [(enqueue! [sieve n step]
                 (let [m (+ n step)]
                   (if (sieve m)
                     (recur sieve m step)
                     (assoc! sieve m step))))
               
               (next-sieve! [sieve candidate]
                 (if-let [step (sieve candidate)]
                   (-> sieve
                       #_ (dissoc! candidate)
                       (enqueue! candidate step))
                   (enqueue! sieve candidate (+ candidate candidate))))
               
               (next-primes [sieve candidate]
                 (if (sieve candidate)
                   (recur (next-sieve! sieve candidate) (+ candidate 2))
                   (cons candidate
                         (lazy-seq (next-primes (next-sieve! sieve candidate)
                                                (+ candidate 2))))))]
         
         (cons 2 (lazy-seq (next-primes (transient (im/int-map)) 3)))))

  ;; but see oprimes for a better way to handle limit
  ([limit]   (take-while #(< % limit) (cg-primes))))



;; don't call dissoc!
;; simpler and faster but doesn't clear old location so probably uses more memory

;; [org.clojure/data.int-map "0.2.4"]
;; (require '[clojure.data.int-map :as im])


;;; skipping dissoc! for performance.
;;; potential for saving memory, but it's slower...

;; this take-while limit does force us to calculate one extra prime
;;
;; could instead seed a zero at the limit to force a stop

;; better limit handling if you want it
;; neg means no limit
;; need to special case limit=1 and 2.  Interally, limit must be marked on an odd candidate
;; since we're skipping evens, but that's not visible to caller.
(defn oprimes
  ([] (oprimes nil))

  ([limit]
   {:pre [(or (nil? limit) (int? limit))]}
   (if (and limit (<= limit 2))
     ()
     (let [update-sieve! (fn [sieve c step]
                           (if (sieve c)
                             (recur sieve (+ c step) step)
                             (assoc! sieve c step)))

           next-primes
           (fn next-primes [sieve candidate]
             (if-let [step (sieve candidate)]
               ;; magic zero step at limit
               (when-not (zero? step)
                 (recur (-> sieve
                            (update-sieve! (+ candidate step) step)
                            ;; better performance without dissoc!, but more memory
                            #_ (dissoc! candidate))
                        (+ candidate 2)))
               (cons candidate (lazy-seq
                                (next-primes (update-sieve! sieve
                                                            (* candidate candidate)
                                                            (* 2 candidate))
                                             (+ candidate 2))))))
           
           composite-map (cond (nil? limit) (im/int-map)
                               (odd? limit) (im/int-map limit 0)
                               ;; marked limit must be odd since we're skipping evens
                               :else (im/int-map (inc limit) 0))]
       
       (cons 2 (lazy-seq (next-primes (transient composite-map) 3)))))))


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


;;; SEM idea: do just odd primes
;;; odd-prime_i = 2i+1
;;; step by 2p to skip missing evens

;; (def unchecked-math *unchecked-math*)
;; (set! *unchecked-math* :warn-on-boxed)

(defn csodd
  "Returns sequence of primes less than N"
  [n]
  (if (<= n 2)
    ()
    (let [nsqrt2 (long (/ (Math/sqrt n) 2.0))]
      (loop [nums (transient (into [2] (range 3 n 2))) i 1]
        (cond
         (> i nsqrt2) (remove nil? (persistent! nums))
         (nums i) (recur (let [step (* 2 (nums i))]
                           (loop [nums nums j (* (nums i) (nums i))]
                             (if (< j n)
                               (recur (assoc! nums (quot j 2) nil) (+ j step))
                               nums)))
                         (inc i))
         :else (recur nums (inc i)))))))

(defmacro aseta [arr i val]
  `(let [arr# ~arr]
     (aset arr# ~i ~val)
     arr#))

(defmacro aremove [f arr]
  `(let [arr# ~(with-meta arr {:tag "[J"})
         f# ~f]
     (persistent! (areduce arr# i# ret#
                           (transient [])
                           (let [x# (aget arr# i#)]
                             (if (f# x#)
                               ret#
                               (conj! ret# x#)))))))

(defn arpm
  "Returns sequence of primes less than N"
  [n]
  (if (<= n 2)
    []
    (let [nsqrt2 (long (/ (Math/sqrt n) 2.0))]
      (loop [nums (long-array (cons 2 (range 3 n 2))) i 1]
        (cond
         (> i nsqrt2) (aremove zero? nums)
         (zero? (aget nums i)) (recur nums (inc i))
         :else (recur (let [p (aget nums i)
                            step (* 2 p)]
                        (loop [nums nums j (* p p)]
                          (if (< j n)
                            (recur (aseta nums (quot j 2) 0) (+ j step))
                            nums)))
                      (inc i)))))))




(def unchecked-math *unchecked-math*)
(set! *unchecked-math* :warn-on-boxed)

;; pretty fast
;; functional style with side-effects on interal long-array
;; but bar-primes is faster
(defn ar-primes
  "Returns sequence of primes less than N"
  [^long n]
  (if (<= n 2)
    []
    (let [nsqrt2 (long (/ (Math/sqrt n) 2.0))]
      (loop [nums (long-array (cons 2 (range 3 n 2))) i 1]
        (cond
         (> i nsqrt2) (persistent! (areduce nums a ret
                                            (transient [])
                                            (let [x (aget nums a)]
                                              (if (zero? x)
                                                ret
                                                (conj! ret x)))))
         (zero? (aget nums i)) (recur nums (inc i))
         :else (recur (let [p (aget nums i)
                            step (* 2 p)]
                        (loop [nums nums j (* p p)]
                          (if (< j n)
                            (recur (do (aset nums (quot j 2) 0) nums) (+ j step))
                            nums)))
                      (inc i)))))))



;; fastest, bar none
;; special case 2 and skip evens
;; mark composites "cmps" as boolean flags

;; building a list backwards is just as fast as using a transient vector and arguably simpler
(defn bar-primes
  "Returns sequence of primes less than N"
  [^long n]
  (if (<= n 2)
    ()
    (let [n (if (odd? n) (dec n) n)
          nsqrt (long (Math/sqrt n))]
      (loop [cmps (boolean-array n) i 3]
        (cond
         (> i nsqrt) (loop [b (dec n) ps ()]
                        (if (< b 3)
                          (conj ps 2)
                          (recur (- b 2) (if (aget cmps b) ps (conj ps b)))))
         (aget cmps i) (recur cmps (+ i 2))
         :else (recur (let [step (* 2 i)]
                        (loop [cmps cmps j (* i i)]
                          (if (< j n)
                            (recur (do (aset cmps j true) cmps) (+ j step))
                            cmps)))
                      (+ i 2)))))))

;; actually a bit slower even though it looks like it should be faster.  Locality of reference?
(defn bar-primes2
  "Returns sequence of primes less than N"
  [^long n]
  (if (<= n 2)
    ()
    (let [n (if (odd? n) (dec n) n)
          nsqrt (long (Math/sqrt n))]
      (loop [cmps (boolean-array n) i 3 ps (transient [2])]
        (cond
         (> i nsqrt) (loop [b i ps ps]
                        (if (< b n)
                          (recur (+ b 2) (if (aget cmps b) ps (conj! ps b)))
                          (persistent! ps)))
         (aget cmps i) (recur cmps (+ i 2) ps)
         :else (recur (let [step (* 2 i)]
                        (loop [cmps cmps j (* i i)]
                          (if (< j n)
                            (recur (do (aset cmps j true) cmps) (+ j step))
                            cmps)))
                      (+ i 2)
                      (conj! ps i)))))))


;; Euler sieve (from Sieve wikipedia page)
;; NB -- you must run the j loop without trashing existing bits -- ie 3 x 9 must still be
;; marked off after you mark 9. Because 9 was still there at the beginning of that round.
;;
;; Slow with sorted-set ~90ms vs ~15ms.
;; Much faster with a im/dense-int-set instead of a normal sorted-set
;; NB: assuming dense-int-set maintains sort ordering even though it's not strictly `sorted?`
;;
;; transducer version was too slow -- deleted.

;; respectable, but nowhere near fast
(defn euler-primes
  "Returns sequence of primes less than N"
  [^long n]
  (if (<= n 2)
    []
    (let [nsqrt (long (Math/ceil (Math/sqrt n)))]
      (loop [nos (into (im/dense-int-set) (range 3 n 2)) ps (transient [2])]
        ;;(println "Nos" nos ps)
        (if-let [s (seq nos)]
          (let [p (long (first s))]
            (if (< p nsqrt)
              (recur (reduce disj (disj nos p) (take-while #(< ^long % n)
                                                           (map #(* p ^long %) s)))
                     (conj! ps p))
              (into (persistent! ps) s)))
          (persistent! ps))))))


;; java.util.BitSet works but isn't as fast as the boolean-array



;; Valentin Waeselynck

(defn vw-primes-below
  "Finds all prime numbers less than n, returns them sorted in a vector"
  [^long n]
  (if (< n 2)
    []
    (let [sieve (boolean-array n false)
          s (-> n Math/sqrt Math/floor int)]
      (loop [p 2]
        (if (> p s)
          (into []
            (remove #(aget sieve %))
            (range 2 n))
          (do
            (when-not (aget sieve p)
              (loop [i (* 2 p)]
                (when (< i n)
                  (aset sieve i true)
                  (recur (+ i p)))))
            (recur (inc p))))))))


(set! *unchecked-math* unchecked-math)




;; (set! *unchecked-math* unchecked-math)


;; Idea about packing odds, sounds like Sundaram
;; https://codereview.stackexchange.com/questions/28902/clojure-code-for-finding-prime-numbers

;; Use packed array of odds, where index i represents value 2i+1. Then you don't have to
;; deal with evens, which are all non-prime a priori (except the 2 of course). Then you can
;; increment by 2*p for a prime p to find its odd multiples twice faster.
;;
;; For a non-marked index i, the prime p is p = 2*i+1, its square is p*p = (2i+1)(2i+1) =
;; 4i^2 + 4i + 1 and its index is (p*p-1)/2 = 2i^2 + 2i = 2i(i+1) = (p-1)(i+1). For the value
;; increment of 2*p, the index increment on 2x-packed array is di = p = 2i+1.



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
  (if (<= limit 2)
    []
    (let [n (quot limit 2)]
      (into [2]
            (comp (remove nil?) (map #(inc (* 2 %))))
            (rest (persistent! (reduce (fn [res x] (assoc! res x nil))
                                       (transient (vec (range n)))
                                       (for [j (range 1 n)
                                             i (range 1 (inc j))
                                             :let [c (+ i j (* 2 i j))]
                                             :while (< c n)]
                                         c))))))))


;; faster with transient int-set and `loop` instead of `for`
(defn sund4 [limit]
  (if (<= limit 2)
    []
    (let [n (quot limit 2)]
      (loop [j 1 cs (transient (into (im/dense-int-set) (range 1 n)))]
        (if (< j n)
          (recur (inc j)
                 (loop [i 1 cs cs]
                   (if (<= i j)
                     (let [c (+ i j (* 2 i j))]
                       (if (< c n)
                         (recur (inc i) (disj! cs c))
                         cs))
                     cs)))
          (cons 2 (map #(inc (* 2 %)) (persistent! cs))))))))


;; one of many at rosettacode.org, unfortunately buggy
;; https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Unbounded_Versions
(defn ROS-primes-to
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (let [max-i (int (/ (- lim 1) 2))
        refs (boolean-array max-i true)
        root (/ (dec (int (Math/sqrt lim))) 2)]
    (do (doseq [i (range 1 (inc root))
                :when (aget refs i)]
          (doseq [j (range (* (+ i i) (inc i)) max-i (+ i i 1))]
            (aset refs j false)))
        (cons 2 (map #(+ % % 1) (filter #(aget refs %) (range 1 max-i)))))))


;; slight fixes by SEM
(defn ROS-primes2
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (if (<= lim 2)
    ()
    (let [max-i (quot lim 2)
          refs (boolean-array max-i true)
          root (/ (dec (int (Math/sqrt lim))) 2)]
      (do (doseq [i (range 1 (inc root))
                  :when (aget refs i)]
            (doseq [j (range (* (+ i i) (inc i)) max-i (+ i i 1))]
              (aset refs j false)))
          (cons 2 (map #(+ % % 1) (filter #(aget refs %) (range 1 max-i))))))))

;; slightly faster to invert boolean logic
(defn ROS-primes3
  "Returns a lazy sequence of prime numbers less than lim"
  [lim]
  (if (<= lim 2)
    ()
    (let [max-i (quot lim 2)
          refs (boolean-array max-i)
          root (/ (dec (int (Math/sqrt lim))) 2)]
      (do (doseq [i (range 1 (inc root))
                  :when (not (aget refs i))]
            (doseq [j (range (* (+ i i) (inc i)) max-i (+ i i 1))]
              (aset refs j true)))
          (cons 2 (map #(+ % % 1) (remove #(aget refs %) (range 1 max-i))))))))



;; highest prime less than 100000
(def phk 99991)


;; for Eric Normand PurelyFunctional.tv newsletter
(defn my-test [f]
  (let [result (f 100000)
        bad (remove nil? (map #(when (not= % %2) %) result primes-300))]
    (assert (empty? bad) (str f " - Bad prime results starting at: " (pr-str (take 5 bad))))
    ;; regression test for small limits, or limit = p^2
    (doseq [i (range 20)]
      (assert (= (classic-sieve i) (f i)) (str f " - Wrong result for " i)))
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
    (c/quick-bench (my-test f))))






;;; ----------------------------------------------------------------------
;;; borrowed from Eric Normand
;;; https://gist.github.com/ericnormand/b29ef113401ad2a6f656c4b701fb08a7




(def tests (clojure.edn/read-string (slurp "resources/normand-tests.edn")))



(comment
  (with-open [w (clojure.java.io/writer "tests.edn")]
    (binding [*print-length* nil]
      (.write w "[")
      (.write w (clojure.string/join "\n  " (map pr-str tests)))
      (.write w "]"))))

(defmacro time2 [expr]
  `(let [t0# (System/nanoTime)
         r# ~expr]
     [(- (System/nanoTime) t0#) r#]))

(def allowed-time
  (* 30 1e9)) ;; 30 seconds in nanoseconds

(defn run-test [f]
  (let [run? (atom true)]
    (doto (Thread.
           (fn []
             (loop [minutes 1]
               (Thread/sleep 60000)
               (when @run?
                 (println minutes "minute")
                 (recur (inc minutes))))))
      .start)
    (try
      (println "Running basic tests.")
      (doseq [[n ex] tests]
        (let [[t ac] (time2 (f n))]
          (assert (= ex ac) (format "Test failed for n=%d." n))
          (assert (<= t allowed-time) (format "Went over allowed time for n=%d." n))))
      (println "Passed tests. Running benchmark.")
      (c/quick-bench (count (f 100000))) ;; make sure it's realized if it's lazy
      (finally
        (reset! run? false))))
  :done)




;; https://en.wikipedia.org/wiki/Truncatable_prime

;; There are 4260 decimal left-truncatable primes
(def largest-left-truncatable-prime 357686312646216567629137N)


;; There are only 83 right-truncatable primes
(def right-truncatable-primes
  [2, 3, 5, 7, 23, 29, 31, 37, 53, 59, 71, 73, 79, 233, 239, 293, 311, 313, 317, 373, 379,
   593, 599, 719, 733, 739, 797, 2333, 2339, 2393, 2399, 2939, 3119, 3137, 3733, 3739, 3793,
   3797, 5939, 7193, 7331, 7333, 7393, 23333, 23339, 23399, 23993, 29399, 31193, 31379,
   37337, 37339, 37397, 59393, 59399, 71933, 73331, 73939, 233993, 239933, 293999, 373379,
   373393, 593933, 593993, 719333, 739391, 739393, 739397, 739399, 2339933, 2399333, 2939999,
   3733799, 5939333, 7393913, 7393931, 7393933, 23399339, 29399999, 37337999, 59393339,
   73939133])


(defn digits [n]
  (keep (fn [ch] (let [d (long ch)] (when (<= (long \0) d (long \9)) (- d (long \0)))))
        (seq  (pr-str n))))

(defn digjoin [ds]
  (reduce (fn [sum d] (+' d (*' sum 10))) 0 ds))

(defn left-truncations [n]
  (map digjoin (take-while seq (iterate rest (digits n)))))

(defn right-truncations [n]
  (map digjoin (take-while seq (iterate pop (vec (digits n))))))

;; NOTE: prime? is not appropriate for bignums.  Too slow for largest left truncatable.
;; Works fine for all the right truncatable examples.
