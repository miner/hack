(ns miner.nestedloop)

;;; https://stackoverflow.com/questions/9137660/how-do-you-replace-java-nested-for-loops-in-clojure

(defn dnolen []
  (last 
   (for [i (range 1000) 
         j (range 1000)
         :let [n (* i j)] 
         :when (and (= (mod n 13) 0) 
                    (= (mod i 7) 0))] 
     n)))

(defn rord []
  (reduce (fn [max-mod [i j]]
            (if (and (zero? (mod (* i j) 13))
                     (zero? (mod i 7)))
              (* i j)
              max-mod))
          0
          (for [i (range 1000) j (range 1000)]
            [i j])))

;;; much faster to run the values in reverse and just take the first.  However, this depends
;;; on the precise problem.  In general you might need to search for the max, best,
;;; whatever, so you need to map/reduce across the whole domain.  On the other hand, it pays
;;; to notice special cases where you can search in the faster order.

(defn rdnolen1 []
  (first
   (for [i (range 999 -1 -1)
         j (range 999 -1 -1)
         :let [n (* i j)] 
         :when (and (zero? (mod n 13))
                    (zero? (mod i 7)))]
     n)))

;;; also much faster to filter the 'i' up front
;;; slight performance enhancement if you use rem instead of mod -- same result if non-neg args
(defn rdnolen2 []
  (first
   (for [i (range 999 -1 -1)
         :when (zero? (rem i 7))
         j (range 999 -1 -1)
         :let [n (* i j)] 
         :when (zero? (rem n 13))]
     n)))


;;; amalloy points out that you can increment by 7 and save a lot of effort
;;; I'm running in reverse so I will decrement by 7.  The initial number has to be
;;; calculated as a multiple of 7.
(defn rdnolen4 []
  (first
   (for [i (range (- 999 (rem 999 7)) -1 -7)
         j (range 999 -1 -1)
         :let [n (* i j)] 
         :when (zero? (rem n 13))]
     n)))


(defn rdnolen5
  ([] (rdnolen5 999))
  ([limit]
   (first
    (for [i (range (- limit (rem limit 7)) -1 -7)
          j (range limit -1 -1)
          :let [n (* i j)]
          :when (zero? (rem n 13))]
      n))))


;;; https://stackoverflow.com/questions/72172428/convert-pseudo-code-with-nested-for-loops-to-clojure

;;; Steffan Westcott writes:

;;; The given pseudocode computes the nth Bernoulli number. It uses all the previous
;;; Bernoulli numbers to compute the result. Much like the factorial function, this lends
;;; itself to a recursive algorithm which may be implemented with memoize to avoid
;;; re-computation of earlier numbers:

(def factorial
  "Returns n!."
  (memoize (fn [n]
             (if (< 1 n)
               (* n (factorial (dec n)))
               1N))))

(def bernoulli
  "Returns the nth Bernoulli number."
  (memoize (fn [n]
             (if (zero? n)
               1
               (let [n!    (factorial n)
                     term  #(/ (* n! (bernoulli %))
                               (factorial %)
                               (factorial (- n % -1)))
                     terms (map term (range n))]
                 (reduce - 0 terms))))))


;; (map bernoulli (range 9))
;; => (1 -1/2 1/6 0N -1/30 0N 1/42 0N -1/30)


;;; SEM: the memoize cache lasts forever (runtime lifetime) which is fine for many cases but
;;; sometimes you want to trade off computation for memory.  You can use the technique of
;;; passing a "recursive call arg" to use instead of the explicit recursion.  I used this
;;; for my mrfn.clj
