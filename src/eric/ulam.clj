(ns eric.ulam)

;; https://gist.github.com/ericnormand/31f4eccd020567c3ecdd8cadd918f725

;; The Ulam sequence is an interesting mathematical sequence of integers. It starts with
;; [1 2]. At each step, the next element is:
;;
;; - not already in the sequence
;; - a sum of two previous elements
;; - the number must be produced by only one sum
;; - the smallest in case there are multiple candidates

;; (take 17 (ulam)) ;=> (1 2 3 4 6 8 11 13 16 18 26 28 36 38 47 48 53)


;;; See   https://oeis.org/A002858

;;; comments from gist point to paper on efficient computation of Ulam numbers in Java:
;;; https://vixra.org/abs/1508.0085



;;; My approach
;;; keep sums in sorted-set -- must be unique
;;; burns are all sums ever and ulams (potential dups) [in a set]
;;; sums are sorted set of unique sums (available for next ulam num)
          

;;; When calculating xsums, we're adding to unique existing nums so sums will also be unique.
;;; Could be an advantage to monotonic increasing order.  How much history really matters?
;;; Not sure how to prune state.

;;; mapv (for xsum) is a winner -- I think it gives us fast path on reduce as well.

(defn ulam []
  (let [ulam-next (fn [state]
                    (let [uset (:uset state)
                          usums (:sums state)
                          burns (:burns state)
                          x (first usums)
                          xsums (mapv #(+ x %) uset)]
                      {:x x
                       :uset (conj uset x)
                       :burns (into (conj burns x) xsums)
                       :sums (reduce (fn [ss z]
                                          (if (burns z)
                                            (disj ss z)
                                            (conj ss z)))
                                        (disj usums x)
                                        xsums)} ))]
    (sequence
     (map :x)
     (cons {:x 1}
           (iterate ulam-next {:x 2
                               :uset #{1 2}
                               :burns #{1 2 3}
                               :sums (sorted-set 3)})))))









;;; https://oeis.org/A002858

(defn smoke-ulam [ulam]
  (assert (= (take 59 (ulam))
             [1, 2, 3, 4, 6, 8, 11, 13, 16, 18, 26, 28, 36, 38, 47, 48, 53, 57, 62, 69, 72,
              77, 82, 87, 97, 99, 102, 106, 114, 126, 131, 138, 145, 148, 155, 175, 177,
              180, 182, 189, 197, 206, 209, 219, 221, 236, 238, 241, 243, 253, 258, 260,
              273, 282, 309, 316, 319, 324, 339]))
  true)










;;; @steffan-westcott  -- faster than mine.  About 580 us on M1 Air.  Mine is about 700 us.

(defn merge-sorted [xs ys]
  (lazy-seq
    (if-let [fx (first xs)]
      (if-let [fy (first ys)]
        (if (< fx fy)
          (cons fx (merge-sorted (rest xs) ys))
          (cons fy (merge-sorted xs (rest ys))))
        xs)
      ys)))

(defn trim-leading-dups [xs]
  (if-let [x (first xs)]
    (if (= x (second xs))
      (recur (drop-while #{x} xs))
      xs)
    ()))

(defn ulam* [ulam-vec sums]
  (lazy-seq
    (let [ulam-next (first sums)
          new-sums (map + ulam-vec (repeat ulam-next))
          sums' (trim-leading-dups (merge-sorted (rest sums) new-sums))]
      (cons ulam-next (ulam* (conj ulam-vec ulam-next) sums')))))

(defn sw-ulam []
  (cons 1 (ulam* [1] [2])))



