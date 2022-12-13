(ns miner.nth-elements)

;;; SEM: This is a bad idea.

;;; https://clojure-diary.gitlab.io/2022/12/05/get-every-nth-element-from-a-sequence-in-clojure.html

(def a-list (range 60))

(defn nth-elements [n coll]
  (map last (partition n coll)))

#_
(nth-elements 12 a-list)

;;;;;;;;;

(def r30 (range 30))

(defn test-nel [nth-elements]
  (assert (= (nth-elements 12 a-list) '(11 23 35 47 59)))
  (assert (= (nth-elements 4 r30) '(3 7 11 15 19 23 27)))
  true)

;; simplest and best
(defn tnel [n coll]
  (take-nth n (drop (dec n) coll)))

(defn xnel [n coll]
  (sequence (comp (drop (dec n)) (take-nth n)) coll))

(defn vnel [n coll]
  (mapv peek (partitionv n coll)))

(defn inel [n coll]
  (into [] (comp (drop (dec n)) (take-nth n)) coll))


(comment
;;; times on MacBook Air M1
(quick-bench (test-nel nth-elements)) ;; Execution time mean : 6.572812 µs
(quick-bench (test-nel tnel))         ;; Execution time mean : 325.880615 ns
(quick-bench (test-nel xnel))         ;; Execution time mean : 2.098459 µs
(quick-bench (test-nel vnel))         ;; Execution time mean : 2.593674 µs
(quick-bench (test-nel inel))         ;; Execution time mean : 1.912267 µs

)
