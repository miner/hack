(ns advent25.greatest
 (:require [clojure.math.combinatorics :as combo]))

;;; I still need to read actual Advent of Code 2025.  This is just from a Reddit post asked
;;; about this problem.

;;; https://www.reddit.com/r/Clojure/comments/1pe6c05/getting_combinations_from_a_nonunique_list_while/

;;; given sequences of potential digits, keep `keeping`, disgarding others but preserving
;;; original order to form largest number.


(def ex '(2 3 4 2 3 4 2 3 4 2 3 4 2 7 8))
(def rex '(4 3 4 2 3 4 2 3 4 2 7 8))


;;; SEM: main insight is that leftmost digits matter most so drop while lower than next
;;; until you've hit your limit.  Consider leftmost window of dropcnt+1.  Must keep max
;;; of first dropcnt+1.  Then start new window after kept digit.

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
