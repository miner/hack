(ns miner.factorial)

;;; looks like the JVM can optimize this so no penalty for recursion
(defn recursive-fact [n]
  (if (zero? n)
    1
    (* n (recursive-fact (dec n)))))

;;; slightly faster.  Usual pattern for Clojure code to recur
(defn loop-fact [n]
  (loop [n n prod 1]
    (if (zero? n)
      1
      (recur (dec n) (long (* n prod))))))

(defn fact [n]
  (reduce * 1 (range 2 (inc n))))

;;; you might want to memoize fact if you're calling it a lot.
(def mfact (memoize fact))





;;; Knuth "Why Pi?"  2010 Christmas Tree lecture
;;; https://www.youtube.com/watch?v=cI6tt9QfRdo


(defn odd-fact [n]
  (reduce * 1 (range 3 (inc n) 2)))

(defn even-fact [n]
  (reduce * 1 (range 2 (inc n) 2)))


;;; Stirling's approximation
;;; n! ~ (n/e)^n * sqrt(2*Pi*n)

(defn stirl [n]
  (let [n (double n)]
    (* (Math/pow (/ n Math/E) n)
       (Math/sqrt (* Math/PI (* 2.0 n))))))




(def pi-50-str "314159265358979323846264338327950288419716939937510")

(def pi-digits (mapv #(- (long %) (long \0)) pi-50-str))

;;; https://oeis.org/A000796

;;; first 105 digits
(def pi-digs [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4, 6, 2, 6, 4, 3, 3,
              8, 3, 2, 7, 9, 5, 0, 2, 8, 8, 4, 1, 9, 7, 1, 6, 9, 3, 9, 9, 3, 7, 5, 1, 0, 5,
              8, 2, 0, 9, 7, 4, 9, 4, 4, 5, 9, 2, 3, 0, 7, 8, 1, 6, 4, 0, 6, 2, 8, 6, 2, 0,
              8, 9, 9, 8, 6, 2, 8, 0, 3, 4, 8, 2, 5, 3, 4, 2, 1, 1, 7, 0, 6, 7, 9, 8, 2, 1,
              4])



;;; Su, Francis E., et al. “Finding the N-th digit of Pi.” Math Fun Facts.
;;; <https://www.math.hmc.edu/funfacts>.

;;; Here is a very interesting formula for pi, discovered by David Bailey, Peter Borwein,
;;; and Simon Plouffe in 1995:

;;; Pi = SUM[k=0 to infinity]  16 ^-k * [ 4/(8k+1) – 2/(8k+4) – 1/(8k+5) – 1/(8k+6) ].

;;; The reason this pi formula is so interesting is because it can be used to calculate the
;;; N-th digit of Pi (in base 16) without having to calculate all of the previous digits!
;;; Moreover, one can even do the calculation in a time that is essentially linear in N,
;;; with memory requirements only logarithmic in N. This is far better than previous
;;; algorithms for finding the N-th digit of Pi, which required keeping track of all the
;;; previous digits!
;;;
;;; The Math Behind the Fact: Here’s a sketch of how the BBP formula can be used to find the
;;; N-th hexadecimal digit of Pi. For simplicity, consider just the first of the sums in the
;;; expression, and multiply this by 16N. We are interested in the fractional part of this
;;; expression. The numerator of a given term in this sum is 16N-k, and it can be evaluated
;;; very easily mod (8k+1) using a binary algorithm for exponentiation. Division by (8k+1)
;;; is straightforward via floating point arithmetic. Not many more than N terms of this sum
;;; need be evaluated, since the numerator decreases very quickly as k gets large so that
;;; terms become negligible. The other sums in the BBP formula are handled similarly. This
;;; yields the hexadecimal expansion of Pi starting at the (N+1)-th digit.



