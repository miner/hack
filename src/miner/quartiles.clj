(ns miner.quartiles)

;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-335-idea-when-can-you-use-property-based-testing/

;; Quartiles
;;
;; q2 is just the median of the dataset. q1 is the median of the lower half of the
;; dataset. And q3 is the median of the upper half of the dataset.  I also like to include
;; q0 and q4, which are the min and max.



;; We have to decide if the median belongs to the lower or upper halves (or neither).
;; I'm using "Tukey's hinges" or what Wikipedia calls "Method 2".  If the original data has an even
;; count, split the data exactly in half.  If it has an odd count, include the median in
;; both halves.  This convention eliminates an awkward edge case for single element data sets.
;;    
;; https://en.wikipedia.org/wiki/Quartile



(defn quartiles [coll]
  "Returns a vector of 5 elements based on the collection of numbers `coll`. Q0 is the
  minimum, Q1 is the median of the lower half, Q2 is the median, Q3 is the median of the
  upper half, and Q4 is the maximum.  Returns nil for the empty collection.  The halves are
  split according to the *Tukey's hinges* convention in which the median belongs to both
  halves if the dataset has an odd number of elements."
  (when (seq coll)
    (let [median-sorted-vec (fn [vvv]
                              (let [c (count vvv)
                                    mid (quot c 2)]
                                (if (odd? c)
                                  (vvv mid)
                                  (/ (+ (vvv (dec mid)) (vvv mid)) 2.0))))
          v (vec (sort coll))
          cnt (count v)
          lower (subvec v 0 (quot (inc cnt) 2))
          upper (subvec v (if (odd? cnt) (dec (count lower)) (count lower)))]
      [(v 0)
       (median-sorted-vec lower)
       (median-sorted-vec v)
       (median-sorted-vec upper)
       (v (dec cnt))])))




(defn smoke-test-quartiles []
  (assert (= (quartiles [6, 7, 15, 36, 39, 40, 41, 42, 43, 47, 49])
             [6 25.5 40 42.5 49]))
  (assert (= (quartiles [7, 15, 36, 39, 40, 41])
             [7 15 37.5 40 41]))
  (assert (= (quartiles [13])
             [13 13 13 13 13]))
  (assert (= (quartiles (range 100001))
             [0 25000 50000 75000 100000]))
  (assert (nil? (quartiles [])))
  true)

          
