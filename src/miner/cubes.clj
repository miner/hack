(ns miner.cubes)

;; https://aperiodical.com/2019/09/42-is-the-answer-to-the-question-what-is-80538738812075974%c2%b3-80435758145817515%c2%b3-12602123297335631%c2%b3/

;; Every cube of a whole number is within one of a multiple of nine, which means that a sum
;; of three cubes must be within three of a multiple of nine. So numbers of the form 9𝑘+4 or
;; 9𝑘+5 cannot be written as the sum of three cubes.
;;
;; In 1992, Roger Heath-Brown conjectured that every other whole number can be written as
;; the sum of three cubes, in infinitely many different ways. Mathematicians on the whole
;; seem to have been convinced by Heath-Brown’s argument that this ought to be true – but
;; actually finding ways to write any particular number as a sum of three cubes remains a
;; difficult problem.

;; A solution for 42 was recently found.  The only remaining unsolved cases up to a thousand
;; are 114, 165, 390, 579, 627, 633, 732, 906, 921, and 975.


;; We may need to use bigints for some calculations.

(defn cube [n] (*' n n n))

(defn sum-cubes [a b c] (long (+' (cube a) (cube b) (cube c))))


(defn possible-three-cubes? [n]
  (case (long (mod n 9))
    (4 5) false
    true))

#_
(sum-cubes −80538738812075974 80435758145817515 12602123297335631)
;;=> 42

#_
(every? #{0 1 8} (map (fn [n] (mod (cube n) 9)) (range 10000)))
;;=> true


#_
(remove possible-three-cubes? (range 100))
;;=> (4 5 13 14 22 23 31 32 40 41 49 50 58 59 67 68 76 77 85 86 94 95)
