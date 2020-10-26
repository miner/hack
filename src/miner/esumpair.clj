(ns miner.esumpair
  (:require [clojure.math.combinatorics :as mc]))


;; https://gist.github.com/ericnormand/c2c94f698bf3ace64c5f722da6dec2fc

;; Write a function that takes a collection of numbers and a target number. Return all pairs
;; of numbers found in the collection that sum up to the target number.  There can be
;; duplicate numbers and hence duplicate pairs.
;; Each pair should be sorted.







(defn smoke-sum
  ([sum-of-pairs]
   (assert
(sums-of-pairs [2 4 5 6] 8) ;=> [[2 6]]
(sums-of-pairs [3 2 0 1 1 5] 5) ;=> [[2 3] [0 5]]
(sums-of-pairs [1 3 2 3 4 5] 7) ;=> [[3 4] [3 4]]
Notes


