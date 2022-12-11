(ns miner.nth-elements)

;;; This is all a bad idea

;;; https://clojure-diary.gitlab.io/2022/12/05/get-every-nth-element-from-a-sequence-in-clojure.html

(def a-list (range 60))

(defn nth-elements [n coll]
  (map last (partition n coll)))

#_
(nth-elements 12 a-list)

;;;;;;;;;

(defn test-nl [nth-elements]
  (assert (= (nth-elements 12 a-list) '(11 23 35 47 59)))
  true)


(defn sem-nl [n coll]
  (take-nth n (drop (dec n) coll)))

(defn sem-nl2 [n coll]
  (sequence (comp (drop (dec n)) (take-nth n)) coll))

(defn sem-nl3 [n coll]
  (mapv peek (partitionv n coll)))

#_
(quick-bench (count (nth-elements 12 a-list)))
;; Execution time mean : 4.665623 µs

#_
(quick-bench (count (sem-nl 12 a-list)))
;; Execution time mean : 121.863602 ns

