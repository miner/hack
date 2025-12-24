(ns advent25.greatest
   (:require [clojure.string :as str])
 (:require [clojure.math.combinatorics :as combo]))

;;; Meta Note:  this is a nicely commented notebook on AOC 2025:
;;; https://narimiran.github.io/aoc2025/


;;; I had not read the actual Advent of Code 2025 when I saw this Reddit post asked
;;; about this problem.  See my dayX.clj files for my AOC work after looking at the real
;;; problems.

;;; https://www.reddit.com/r/Clojure/comments/1pe6c05/getting_combinations_from_a_nonunique_list_while/

;;; given sequences of potential digits, keep `keeping`, discarding others but preserving
;;; original order to form largest number.


(def ex '(2 3 4 2 3 4 2 3 4 2 3 4 2 7 8))
(def rex '(4 3 4 2 3 4 2 3 4 2 7 8))


;;; SEM: main insight is that leftmost digits matter most so drop while lower than next
;;; until you've hit your limit.  Consider leftmost window of dropcnt+1.  Must keep first
;;; max of dropcnt+1 window.  Then start new window after kept digit.

;;; faster to subvec and manage dropping precisely

(defn gnum [digits keeping]
  (loop [dv (vec digits) dropping (- (count dv) keeping) res []]
    (cond (= (count res) keeping)  res
          (pos? dropping)
              ;; dg is [drop-count greatest] for the leftmost window on dv
              (let [[d g] (reduce-kv (fn [dg i x] (if (> x (peek dg)) [i x] dg))
                                        [-1 -1]
                                        (subvec dv 0 (inc dropping)))]
                (recur (subvec dv (inc d)) (- dropping d) (conj res g)))
          :else (into res dv))))


(defn smoke-gnum
  ([] (smoke-gnum gnum))
  ([gnum]
   (assert (= (gnum ex 12) rex))
   (assert (= (gnum [1 1 1 1 0 4] 2) [1 4]))
   (assert (= (gnum [1 1 1 1 1 1] 2) [1 1]))
   (assert (= (gnum [1 2 3 1 1 1 3 4 5 6 7 8] 8) [3 1 3 4 5 6 7 8]))
   (assert (= (gnum [4 6 7 8 9 2 3 4 5 2 3 1 1 1 9 3 7 8 1] 10)
              [9 5 3 1 1 9 3 7 8 1]))
   (assert (= (gnum [4 6 7 8 9 2 3 4 5 2 3 1 1 1 9 3 7 8
                     1 4 6 7 8 9 2 3 4 5 2 3 1 1 1 9 3 7 8 1] 20)
           [9 9 8 7 8 9 2 3 4 5 2 3 1 1 1 9 3 7 8 1]))
   true))

;;; Much slower approach:
;;; https://github.com/jflinchbaugh/aoc2025/blob/220792c8c2133fd0009015e61d9f6ca6ecf0f44a/src/aoc2025/day_3.clj#L6



;;; 12/21/25  06:57 by miner -- related problem with a twist.

;;; what about finding the smallest number instead of greatest?
;;; Twist: no leading zeros allowed.  Change the test-xp function as you loop

(defn digitize [n]
  (assert (not (neg? n)))
  (mapv #(bit-and 2r1111 (long %)) (str n)))


;; faster
(defn dv->long [dv]
  (reduce (fn [r d] (+ d (* 10 r))) 0 dv))


;; Inside the loop, [d lo] is drop-count 'd' to get to lowest digit 'lo' for the leftmost
;; window on dv.

(defn least-num [n keeping]
  (assert (and (not (neg? n)) (pos? keeping)))
  (loop [dv (digitize n)
         dropping (- (count dv) keeping)
         res []
         ;; ltxp is the less-than test where x is the element dv and p is the previous lowest
         ltxp (if (= keeping 1) < (fn [x p] (and (< x p) (pos? x))))]
    (cond (= (count res) keeping)  (dv->long res)
          (pos? dropping) (let [[d lo] (reduce-kv (fn [dl i x] (if (ltxp x (peek dl)) [i x] dl))
                                                  [-1 10]
                                                  (subvec dv 0 (inc dropping)))]
                            (recur (subvec dv (inc d)) (- dropping d) (conj res lo) <))
          :else (when (= (+ (count res) (count dv)) keeping)
                  (dv->long (into res dv))))))


(defn smoke-lnum
  ([] (smoke-lnum least-num))
  ([least-num]
   (assert (= (least-num 9279999456300142031 15) 279456300142031))
   (assert (= (least-num 0 1) 0))
   (assert (= (least-num 901 1) 0))
   (assert (= (least-num 901 2) 90))
   (assert (= (least-num 901 3) 901))
   (assert (nil? (least-num 901 4)))
   (assert (= (mapv #(least-num 90817 %) (range 1 7)) [0 17 817 9017 90817 nil]))
   (assert (= (mapv #(least-num 9876019002 %) (range 1 13))
              [0 10 100 1002 19002 601002 6019002 76019002 876019002 9876019002 nil nil]))
   true))

          




;; slower
(defn longv2 [dv]
  (parse-long (apply str dv)))

;; a bit slower and less clear
(defn dgt [n]
  (assert (not (neg? n)))
  (if (zero? n)
    [0]
    (loop [n n res ()]
      (if (zero? n)
        (vec res)
        (recur (quot n 10) (conj res (rem n 10)))))))

;; slower
(defn digitize2 [n]
  (assert (not (neg? n)))
  (if (zero? n)
    [0]
    (loop [res nil n n]
      (if (zero? n)
        (vec res)
        (recur (conj res (rem n 10)) (quot n 10))))))

;; about the same speed as digitize but not as nice
(defn digitize3 [n]
  (mapv #(case % \0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 nil)
        (str n)))
