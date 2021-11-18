(ns eric.ulam)

;; https://gist.github.com/ericnormand/31f4eccd020567c3ecdd8cadd918f725

;; The Ulam sequence is an interesting mathematical sequence of integers. It starts with
;; [1 2]. At each step, the next element is:
;;
;; - not already in the sequence
;; - a sum of two previous elements
;; - the number must be produced by only one sum
;; - the smallest in case there are multiple candidates




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

;;; why doesn't this work?  Probably slow, but there's something else happening here?  This
;;; goes deeper looking for dupes throughout full collection.  That's probably OK -- I
;;; suspect that a later merger is causing the problem.
(defn remove-dups [coll]
  (sequence (comp (partition-by identity)
                  (mapcat #(when (empty? (rest %)) %)))
            coll))

;;; drops any leading duplicates -- but doesn't go past first single value
(defn drop-while-dups [coll]
  (when-first [x coll]
    (if (= (second coll) x)
      (recur (drop-while #(= x %) (nnext coll)))
      coll)))

;; dropping dups
;; by construction, colls are pre-sorted and uniqued.
;; BUG-- they might already have dups!

#_
(defn BUG-my-merge-sums [xs ys]
  (lazy-seq
   #_ (when (= (first xs) (first ys))
     (println "Collision") (println "  " xs) (println "   " ys) (println))
              
   (cond (empty? xs) ys
         (empty? ys) xs
         ;;(= (first xs) (first ys)) (my-merge-sums (next xs) (next ys))
         (< (first xs) (first ys)) (cons (first xs) (my-merge-sums (rest xs) ys))
         :else (cons (first ys) (my-merge-sums xs (rest ys))))))

(defn merge-sums [xs ys]
  (lazy-seq
   (cond (empty? xs) ys
         (empty? ys) xs
         (< (first xs) (first ys)) (cons (first xs) (merge-sums (rest xs) ys))
         :else (cons (first ys) (merge-sums xs (rest ys))))))

;;; simpler? but not faster
(defn ulam7 []
  (let [ulam-next (fn [state]
                    (let [usums (:sums state)
                          uv (:uv state)
                          x (first usums)
                          xsums (map #(+ x %) uv)]
                      {:uv (conj uv x)
                       :sums (drop-while-dups (merge-sums (rest usums) xsums))}))]
    (sequence
     (map (comp peek :uv))
     (cons {:uv [1]}
           (iterate ulam-next {:uv [1 2] :sums '(3)})))))


;; FAILS
(defn ulam8-BUG []
  (let [ulam-next (fn [state]
                    (let [usums (:sums state)
                          uv (:uv state)
                          x (first usums)
                          xsums (map #(+ x %) uv)]
                      {:uv (conj uv x)
                       :sums (remove-dups (merge-sums (rest usums) xsums))}))]
    (sequence
     (map (comp peek :uv))
     (cons {:uv [1]}
           (iterate ulam-next {:uv [1 2] :sums '(3)})))))



;;; https://oeis.org/A002858

(defn smoke-ulam [ulam]
  (assert (= (take 59 (ulam))
             [1, 2, 3, 4, 6, 8, 11, 13, 16, 18, 26, 28, 36, 38, 47, 48, 53, 57, 62, 69, 72,
              77, 82, 87, 97, 99, 102, 106, 114, 126, 131, 138, 145, 148, 155, 175, 177,
              180, 182, 189, 197, 206, 209, 219, 221, 236, 238, 241, 243, 253, 258, 260,
              273, 282, 309, 316, 319, 324, 339]))
  true)

(defn smoke-ulam17 [ulam]
  (assert (= (take 17 (ulam)) '(1 2 3 4 6 8 11 13 16 18 26 28 36 38 47 48 53)))
  true)


;;; Test with a smoke17
;;; Consider lazy burns, as needed, maybe check later rather than for each iteration across
;;; the board.






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





;;; much slower
(defn nexul [us]
  (->> (for [a us b us :when (< a b)] (+ a b))
       frequencies
       (filter #(= 1 (val %)))
       (map first)
       (remove (set us))
       (apply min)
       (conj us)))

(defn mc-ulam []
  (->> (iterate nexul [1 2])
       (map last)
       (cons 1)))
