(ns miner.lynchbell
  (:require [clojure.math.combinatorics :as mc]))

;; Puzzle for largest Lynch-Bell number
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-327-tip-always-be-decomplecting/
;; "A Lynch-Bell number is an integer that is divisible by each of its base-ten digits. The
;; digits have to be all different. ... Implement a search for the largest Lynch-Bell number."


;; Independent discussion on Stack Exchange:
;; https://math.stackexchange.com/questions/2766109/biggest-lynch-bell-number#2766151
;;
;; My paraphrase:  Zero is excluded by definition.  Similarly, you can't have both 2 and
;; 5 (2x5=10).  So you know there are no 10 or 9 digit LBs.  Any even and 5 would also be a
;; problem producing a factor of 10 and an ending 0 digit.  So 5 is out.  Leaving 98764321
;; possible in some order.  But the sum of digits is 40 which means it can't be multiple of
;; 9 (or 3).  So no eight digit LBs.
;;
;; We want to start with a 9 if possible.  That also means the LB is a multiple of 9.  So
;; the sum of the digits must be a multiple of 9.  The failing eight-digit sum was 40.  We
;; notice that 9x4 = 36 = 40 - 4, which suggests excluding 4.  That leaves a seven-digit set
;; of 9876321 (in some order) to try.
;;
;; My comments:  Excluding 0 is obviously necessary.  Excluding 5 makes sense to preserve
;; the even numbers but that's not strictly proven (by me).  Excluding 4 is based on the
;; desire to have 9 but really needs more proof than I give here.  See the link for a full
;; treatment.


;; It is known
(def largest-lynch-bell 9867312)

(def examples [7 126 3915 largest-lynch-bell])

;; https://oeis.org/A115569
(def lb-seq [1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 15, 24, 36, 48, 124, 126, 128, 132, 135, 162,
             168, 175, 184, 216, 248, 264, 312, 315, 324, 384, 396, 412, 432, 612, 624,
             648, 672, 728, 735, 784, 816, 824, 864, 936, 1236, 1248, 1296, 1326, 1362,
             1368, 1395, 1632, 1692, 1764, 1824])
  

;; zero is excluded
(defn zmod? [n d]
  (and (pos-int? n)
       (pos-int? d)
       (zero? (rem n d))))

(defn digits [n]
  (map #(- (long %) (long \0)) (str n)))

(defn digs->long [digs]
  (reduce #(+ (* 10 ^long %) ^long %2) 0 digs))

(defn lb-digits? [n digs]
  (and (apply distinct? digs)
       (every? #(zmod? n %) digs)))

(defn lb? [digs]
  (lb-digits? (digs->long digs) digs))

(defn lynch-bell? [n]
  (lb-digits? n (digits n)))

;; Note: mc/permutations guarantees lexical ordering so we seed digs with the largest digits
;; first.  That way, we test larger numbers first and stop on the first success.
(defn search-lb [digs]
  (when-let [success (first (filter lb? (mc/permutations digs)))]
    (digs->long success)))

;; returns collection of digits in high to low orders (9 ... 1) excluding args
(defn seed-excluding [& excluding]
  (remove (set excluding) (range 9 0 -1)))

;; From the previous discussion, we conclude that the search must start with seven digits.
;; The `seed-excluding` function returns a "seed" collection of digits with the given digits
;; excluded.  We're pretty sure 5 should be excluded so we need to exclude one more to make
;; seven digits.

(defn find-largest-lynch-bell []
  (first (keep search-lb (map seed-excluding (repeat 5) [1 2 3 4 6 7 8 9]))))

#_
(time (find-largest-lynch-bell))
;; "Elapsed time: 43.613458 msecs"
;; 9867312


;; Trying to keep the 9, we note that excluding the 4 gives us seven digits 9876321, whose
;; sum is 36 (a multiple of 9).  We use that as an initial seed and find success quickly.
;; I don't provide a proof that this short-cut is 100% correct, but it has the virtue of
;; yielding the right answer.  [The link to StackExchange explains why 4 should be
;; excluded.]

(defn fast-largest-lynch-bell []
  (search-lb [9 8 7 6 3 2 1]))

#_
(time (fast-largest-lynch-bell))
;; "Elapsed time: 0.313049 msecs"
;; 9867312



(defn smoke []
  (assert (every? lynch-bell? lb-seq))
  (assert (= (find-largest-lynch-bell) largest-lynch-bell))
  (assert (= (fast-largest-lynch-bell) largest-lynch-bell))
  (println "Find largest Lynch-Bell number")
  (time (find-largest-lynch-bell))
  (println)
  (println "Fast")
  (time (fast-largest-lynch-bell)))

