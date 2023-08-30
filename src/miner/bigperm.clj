(ns miner.bigperm
  (:require [clojure.math.combinatorics :as c]))


;;; https://blog.klipse.tech/clojure/2016/09/16/combinatorics-riddle.html

;;; Riddle: You’ve got the first 6 digits 1,2,3,4,5 and 6.
;;;
;;; You have to partition the digits into 3 numbers x, y and z where:
;;;
;;; x is a 3-digit number
;;; y is a 2-digit number
;;; z is a single digit number
;;; Such that the multiplication x*y*z is maximal.

;;; example by Yehonathan Sharvit, but obviously doesn't return x,y,z?  Must have been an
;;; editing error.
(defn sharvit-example []
  (apply max-key (fn [[a b c d e f]]
                 (* (+ (* 100 a) (* 10 b) c)
                    (+ (* 10 d) e)
                    f))
         (c/permutations (range 1 7))))

;;; rewriting to be more sensible
(defn semsol []
  (transduce (map (fn [[a b c d e z]]
                    (let [x (+ (* 100 a) (* 10 b) c)
                          y (+ (* 10 d) e)]
                      [x y z (* x y z)])))
             (fn ([r v] (if (>= (peek v) (peek r)) v r))
               ([r] (pop r)))
             [0 0 0 0]
             (c/permutations (range 1 7))))


(defn semsol3 []
  (transduce (map (fn [[a b c d e z]]
                    (let [x (+ (* 100 a) (* 10 b) c)
                          y (+ (* 10 d) e)]
                      [x y z (* x y z)])))
             (fn ([r v] (max-key peek r v))
               ([r] (pop r)))
             [0 0 0 0]
             (c/permutations (range 1 7))))


(defn semsol2 []
  (pop (apply max-key peek (map (fn [[a b c d e z]]
                                  (let [x (+ (* 100 a) (* 10 b) c)
                                        y (+ (* 10 d) e)]
                                    [x y z (* x y z)]))
                                (c/permutations (range 1 7))))))



;;; Of course, it is inefficient to generate and test all the permutations.  As we're trying
;;; to maximize the product, you need to maximize the factors.  So the biggest digit goes to
;;; the highest places, smaller factor, first.  Well, not so obvious, when you get to the
;;; tens places.  You have to allocate 6, 5y, 4xx first.  Then decide the 3 has to go in the
;;; 10x, and the 2 goes to the y, because???  Leaving the 1 for x.  Something like that.  I
;;; guess you do need to generate and test a bit.


;;; INCOMPLETE reasoning...

;;; The right way to do it would be to write out the long multiplication calculations by
;;; digit.

;;; (* abc (* de f))

;;; (* de f) = (+ (* 10 d f) (* e f)) = [df][ef]

(comment
  
 (+ (* 100 a (* e f))
    (*  10 b (* e f))
    (*     c (* e f))
    (*  10 100 d f a)
    (*  10 10 d f b)
    (*  10 d f c))

 )

(defn simple-sol []
  (let [[a b c d e f] (range 1 7)
        z f
        y (+ (* 10 e) b)
        x (+ (* 100 d) (* 10 c) a)]
    [x y z]))


;;; trying to work out how significant each digit is
    
#_ (expow '[a b c])
;;=> [[c 1] [b 10] [a 100]]

(defn expow [v]
  (mapv vector (rseq v) (iterate #(* % 10) 1)))

(defn exmult
  ([] [])
  ([v] (expow v))
  ([v w]
   (vec (for [x (expow v)
              y (expow w)]
          (-> [(* (peek x) (peek y))]
              (into (pop x))
              (into (pop y))))))
  ([v w u]
   (vec (for [x (expow v)
              y (expow w)
              z (expow u)]
          (-> [(* (peek x) (peek y) (peek z))]
              (into (pop x))
              (into (pop y))
              (into (pop z)))))))

;;; UNFINISHED  mult with carry
#_
(defn exmultc
  ([] [])
  ([v] (expow v))
  ([v w]
   (vec (for [x (expow v)
              y (expow w)]
          (-> [(* (peek x) (peek y))]
              (into (pop x))
              (into (pop y))))))
  ([v w u]
   (vec (for [x (expow v)
              y (expow w)
              z (expow u)]
          (-> [(* (peek x) (peek y) (peek z))]
              (into (pop x))
              (into (pop y))
              (into (pop z)))))))

(defn revalue [xv]
  (reduce (fn [m [k & vs]] (reduce (fn [r v] (assoc r v (+ (get r v 0) k))) m vs))
          {}
          xv))

#_ (exmult '[a b c] '[d e] '[f])
;;=> [[1 c e f] [10 c d f] [10 b e f] [100 b d f] [100 a e f] [1000 a d f]]

#_ (revalue '[[1 c e f] [10 c d f] [10 b e f] [100 b d f] [100 a e f] [1000 a d f]])
;;=> {c 11, e 111, f 1221, d 1110, b 110, a 1100}

#_ (sort-by val *1)
;;=> ([c 11] [b 110] [e 111] [a 1100] [d 1110] [f 1221])

;; so F should be highest digit as it gets most factorized
;; D, A, E, B, C smallest
;;;; BUT MY REASONING DOES NOT WORK
;;; Why isn't E bigger than B ???

;;; Maybe my calc was wrong?  Maybe logic is wrong?

;;;; FIXME: Try it with explicit carrying



(defn qval [[a b c d e f]]
  (+ (* a 1100) (* b 110) (* c 11)
     (* d 1110) (* e 111)
     (* f 1221)))

;;; ABC 431   DE 52   F 6


(defn combine-digits [[a b c d e z]]
  [(+ (* 100 a) (* 10 b) c)
   (+ (* 10 d) e)
   z])

(defn qsol []
  (combine-digits (apply max-key qval (c/permutations (range 1 7)))))
