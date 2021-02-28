(ns miner.sexag)

;; https://en.wikipedia.org/wiki/Sexagesimal

;; Sexagesimal, also known as base 60 or sexagenary,[1] is a numeral system with sixty as
;; its base. It originated with the ancient Sumerians in the 3rd millennium BC, was passed
;; down to the ancient Babylonians, and is still used—in a modified form—for measuring time,
;; angles, and geographic coordinates.

;; Knuth paper "Ancient Babylonian Algorithms"
;; http://www.realtechsupport.org/UB/NP/Numeracy_BabylonianAlgorithms_1977.pdf


;; Various notations have been used, mixing our arabic numerals.
;; Neugebauer notation (as used by Wikipedia) is  A;B,C,D
;; where A is the whole number (decimal)
;; B is the "seconds" B/60
;; C is C/(60x60), D etc 60^-N

;; Knuth just used commas as separators.  First number is whole + same fractions of 60.

;; For Clojure, it seems a vector would be convenient to use with a data-reader.
;; #sx [3 7 30]   ==>  25/8  (ratio ~ 3.125)

(defn sexag [v]
  (reduce-kv (fn [sum d n]
               (+ sum (/ n (apply * (repeat d 60)))))
             0
             v))



(defn sx7 [v]
  (reduce + (map / v (iterate #(* % 60) 1))))


;; assuming we never need more than 10 places
(def pow60 (vec (take 10 (iterate #(* % 60) 1))))

;; fastest
(defn tx7 [v]
  (transduce (map-indexed (fn [i n] (/ n (pow60 i))))
              +
              0
              v))



(def dpow60 (vec (take 10 (iterate #(* % 60.0) 1.0))))
(defn dsx72 [v]
  (reduce + (map / v dpow60)))

;; fast if you can use double math instead of (exact) ratios
(defn dtx72 [v]
  (transduce (map-indexed (fn [i n] (/ n (dpow60 i))))
              +
              0.0
              v))

;; actually slower than dtx72
(defn dtx73 [v]
  (let [vs (seq v)]
  (transduce (map-indexed (fn [i n] (/ n (dpow60 (inc i)))))
              +
              (first vs)
              (rest vs))))


;; surprisingly not faster
(defn sx71 [v]
  (reduce + (map / v pow60)))


(def inv60 (vec (take 10 (iterate #(/ % 60) 1))))

;; a bit slower
(defn tx8 [v]
  (transduce (map-indexed (fn [i n] (* n (inv60 i))))
              +
              0
              v))






;; slower but concise
(defn sx4 [v]
  (reduce + (map * v (iterate #(/ % 60) 1))))


;; even slower
(defn sx6 [v]
  (reduce + (first v) (map-indexed (fn [d n]
                                     (apply / n (repeat (inc d) 60)))
                                   (rest v))))

;; At first, it looked like the * wasn't necessary but it really is.
;; Note (/ n 60 60) works but (/ n) is (/ 1 n) so the zero place was incorrect


;; slower to special case
(defn sexag2 [v]
  (reduce-kv (fn [sum d n]
               (if (zero? d)
                 sum
                 (+ sum (apply / n (repeat d 60)))))
             (first v)
             v))

;; how do you convert back to sexagesimal?
;; Whole number (long?) is just that.  For a ratio?  Figure out the denominator



#_   ;; BUG with rkv subv
(defn sexag3 [v]
  (reduce-kv (fn [sum d n]
               (+ sum (/ n (apply * (repeat (inc d) 60)))))
             (first v)
             (subvec v 1)))


;; transducer version slightly slower
(defn tsexag [v]
  (transduce (map-indexed (fn [d n] (/ n (apply * (repeat d 60)))))
             +
             0
             v))



