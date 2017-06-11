;; http://www.vdare.com/articles/derbs-december-diary
;; 
;; A brainteaser, passed on by a friend. Says she (yes, my friend is a Gyno-American): “It’s
;; of the Diophantinevariety — more unknowns than equations but with only integer
;; solutions.”
;; 
;; I found pennies, nickels and dimes for a total of 18 coins. The total monetary value was
;; 73 cents. How many of each denomination did I find?
;; 
;; As usual in math there is an elegant solution and a merely enumerative one. Go for elegance.
;; 

;; Code didn't help.

(defn success? [cnt val dime nick pen]
  (and (= cnt (+ dime nick pen))
       (= val (+ (* 10 dime) (* 5 nick) pen))))

(def puzz? (partial success? 18 73))




;; c = d + n + p = 18
;; v = 10d + 5n + p = 73
;; 
;; all variables are natural numbers (nonneg integers)
;; 
;; My reasoning
;; p = 3 + 5i .... (3 8 13 18)
;; p obviously less than 18
;; 
;; v-p = 10d + 5n
;; p = 13, leaves v-p = 60 and d+n = 5, 5d = 50 so not enough
;; p = 8, leaves v-p = 65 and d+n = 10, 10d = 100, 10n = 50 so might work
;; d = 3, leaves (v-10d-p) = 5n = 35, so n = 7
;; 
;; solution: 3 dimes, 7 nickles, 8 pennies
;; 
;; check:
;; 3 + 7 + 8 = 18
;; 30 + 35 + 8 = 73
;; 
;;     
;; 
;; 
;; p = 3, leaves 70 and d+n=15, 15n = 75 so too much.
