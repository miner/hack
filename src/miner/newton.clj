(ns miner.newton)

;; stackoverflow question
;;
;; https://stackoverflow.com/questions/49351969/square-root-approximation-clojure
;;
;; original poster had function declarations in wrong order.  Easy to fix.  But I wonder
;; about the double recursion.  Can it be made into tail recursion?  Not easily.  Unless you
;; go back to the original defnition -- then it's simple.


;; Questioner's code first...

;; very slow
(defn aprox [n prox part]
  (if (= part 20)
    prox
    (+ (/ n (* 2 (aprox n part (+ part 1)))) (/ (aprox n prox (+ part 1)) 2)))  )

;; my SLOW-.   Don't even try it.  Also, it's a bug in the first part
(defn SLOW-msqrt [n]
  (aprox  n 1.0 1))


;; (msqrt 9)


;; slightly revised, but still very slow

(defn approx2
  ([n] (approx2 n 1.0 1))
  ([n prox part]
  (if (= part 20)
    prox
    (+ (/ n (* 2 (approx2 n part (inc part))))
       (/ (approx2 n prox (inc part)) 2)))  ))


;; a pretty good answer for the question
(defn msqrt1 [n]
  (loop [prox 1.0, part 20]
    (if (pos? part)
      (recur (+ (/ n (* 2 prox)) (/ prox 2)) (dec part))
      prox)))

;; my revision -- but there's a performance cost to the multi-arity 2x
(defn msqrt2
  ([n] (msqrt2 n 1.0 20))
  ([n prox part]
    (if (pos? part)
      (recur n (/ (+ prox (/ n prox)) 2.0) (dec part))
      prox)))


;; I like this slightly better, but that's just a matter of taste
(defn msqrt4 [n] 
  (loop [prox 1.0 part 20]
    (if (zero? part)
      prox
      (recur (/ (+ prox (/ n prox)) 2.0) (dec part)))))



;; ------------------------

;; My answer:

;; https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method

;; I remember this as Newton's method.  However, Wikipedia says it should be called the
;; Babylonian method as they devised this way of calculating square roots about 1600 years
;; before Newton.  The Babylonian method can be considered a specialized version of
;; Netwon's more general method for finding roots of polynomials.

;; Basic idea:  f(x) = x^2 - N, solve for root = 0, x = sqrt(N)
;; My notation without subscripts: x is really x_i, x+ is really x_i+1
;; geometrically, from calculus:
;; x+ = x - f(x)/f'(x)
;; x+ = x - [x^2 - N] / 2x
;; x+ = [x + N/x]/2

(defn newt
  ([n] (newt n 1.0 20))
  ([n guess steps]
   (if (zero? steps)
     guess
     (recur n (/ (+ guess (/ n guess)) 2.0) (dec steps)))))

;; SEM:  there's a 2x performance penalty to multi-arity
(defn newt
  ([n] (newt n 1.0 20))
  ([n guess steps]
   (if (zero? steps)
     guess
     (recur n (/ (+ guess (/ n guess)) 2.0) (dec steps)))))

;; fastest, loop with init constants seems to be faster than multi-arity defaults
(defn newt4 [n]
  (loop [guess 1.0 steps 20]
    (if (zero? steps)
      guess
      (recur (/ (+ guess (/ n guess)) 2.0) (dec steps)))))


(defn nt3 [n guess steps]
  (if (zero? steps)
    guess
    (recur n (/ (+ guess (/ n guess)) 2.0) (dec steps))))

(defn nt1 [n] (nt3 n 1.0 20))


(defn newt-step [n guess]
  (/ (+ guess (/ n guess)) 2.0))

(defn newt20 [n]
  (take 20 (iterate #(newt-step n %) 1.0)))


;; rnewt is twice as fast as newt, but not as fast as newt4
;; iterate is lazy, but fast enough that it's worth stopping early on convergence
(defn rnewt [n]
  (reduce (fn [^double best ^double guess]
            (if (< (Math/abs (- best guess)) (double Float/MIN_VALUE))
              (reduced guess)
              guess))
          0.0
          (iterate #(newt-step n %) 1.0)))

(defn newt3
  ([n] (newt3 n 1.0 20))
  ([n guess steps]
   (loop [guess guess steps steps]
     (if (zero? steps)
       guess
       (recur (/ (+ guess (/ n guess)) 2.0) (dec steps))))))

;; in practice 11 steps are good enough for perfect squares up to 10000.  That is, you get an
;; exact answer that's correct.  Need to test approximates for non-perfects.  For numbers
;; around 10000, 15 steps seems fine.  But there's no speed benefit to stopping early
;; because the cost of the check is about as much as the extra work saved.
(defn newt2
  ([n] (newt2 n 1.0 20))
  ([n guess steps]
   (cond (zero? steps) guess
         (< (Math/abs ^double (- (* guess guess) n)) Float/MIN_VALUE)
             (do #_ (println "exact" n steps) guess)
         :else  (recur n (/ (+ guess (/ n guess)) 2.0) (dec steps)))))


(comment

 (= (map newt (map #(* % %) (range 1 101)))  (range 1.0 101.0 1.0))

 )


;; Not worth using
(defn estimate [n]
  (if (< n 4)
    1.0
    (double (inc (Long/numberOfTrailingZeros (Long/highestOneBit n))))))

(defn estimate [n]
  (if (< n 4)
    1.0
    (double (- 64 (Long/numberOfLeadingZeros n)))))


;; https://en.wikipedia.org/wiki/Integer_square_root
;; bitwise solution
