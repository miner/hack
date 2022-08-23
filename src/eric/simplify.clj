(ns eric.simplify)

;;; https://gist.github.com/ericnormand/8698a9a60fcfebb3e6cf8953d710700f

;;; Write a function simplify that takes two integers (representing the numerator and
;;; denominator) and simplifies the fraction they represent, returning the two numbers.

(defn smoke-sim [simplify]
  ;; the fraction 10/10
  (assert (= (simplify 10 10) [1 1]))
  ;; the fraction 1/3
  (assert (= (simplify 1 3) [1 3]))
  (assert (= (simplify 2 4) [1 2]))
  (assert (= (simplify 100 40) [5 2]))
  (assert (= (simplify (* 2 3 5 7 11 17) (* 2 3 3 5 5 7 11))) [17 15])
  true)

;;; assuming non-neg numerator, non-zero denominator
(defn simplify [num denom]
  (let [g (loop [a num b denom] (if (zero? b) a (recur b (rem a b))))]
    [(quot num g) (quot denom g)]))

;; clojure standard way, handles negs, zero
(defn simp [num denom]
  (when-not (zero? denom)
    (let [r (/ num denom)]
      (if (ratio? r)
        [(numerator r) (denominator r)]
        [r 1]))))


;; non-neg a,b
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

;; assuming non-neg
(defn simp1 [num denom]
  (assert (and (pos? denom) (not (neg? num))))
  (let [g (gcd num denom)]
    [(quot num g) (quot denom g)]))





(defn simp2 [num denom]
  (cond (zero? denom) nil
        (zero? num) [0 1]
        (and (neg? num) (neg? denom)) (simplify (- num) (- denom))
        (neg? num) (update (simplify (- num) denom) 0 -)
        (neg? denom) (update (simplify num (- denom)) 0 -)
        :else (simplify num denom)))

  
(defn simp3 [num denom]
  (when-not (zero? denom)
    (let [gcd (fn [a b] (if (zero? b) a (recur b (rem a b))))
          g (gcd (abs num) (abs denom))]
      (cond (zero? num) [0 1]
            (and (neg? num) (neg? denom)) [(quot (- num) g) (quot (- denom) g)]
            (neg? num)  [(- (quot (- num) g)) (quot denom g)]
            (neg? denom) [(- (quot num g)) (quot (- denom) g)]
            :else [(quot num g) (quot denom g)]))))

;;; ----------------------------------------------------------------------

;;; @jonasseglare -- I like this one!  Integrates gcd into logic
(defn jo-simplify [a b]
  (loop [x a
         y b]
    (if (zero? y)
      [(/ a x) (/ b x)]
      (recur y (mod x y)))))
    
;;; SEM hack, a bit faster
(defn jo2-simplify [a b]
  (loop [x a y b]
    (if (zero? y)
      [(quot a x) (quot b x)]
      (recur y (rem x y)))))

;;; being more careful about sign
(defn jo3-simplify [a b]
  (when-not (zero? b)
    (let [negative (not= (neg? a) (neg? b))
          a (abs a)
          b (abs b)]
      (loop [x a y b]
        (if (zero? y)
          [(if negative (- (quot a x)) (quot a x)) (quot b x)]
          (recur y (rem x y)))))))
