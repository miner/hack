(ns miner.eric2
  (:require [clojure.math.combinatorics :as mc]))


;; ----------------------------------------------------------------------
;; 11/18/19  09:00 by miner -- new problem
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-352-tip-use-the-right-kind-of-comment-for-the-job/
;;
;; Write a program that outputs all possibilities to put a + or - or nothing between the
;; numbers 1-9 (in order) such that the result is 100. For example:
;;
;; 1 + 2 + 34 - 5 + 67 - 8 + 9 = 100
;; The function should output a list of strings with these formulas.


;; We need to evaluate all the solutions so no need to be lazy.  The fn-prev-x is like a
;; reducing function, where prev is the previous result (a vector) and x is the new item to
;; be considered.  It should return a new "prev" result or accumuation of numbers.

(defn expand-nums [fn-prev-x coll]
  (letfn [(gen [acc prev coll]
            (if (seq coll)
              (-> acc
                  (gen (conj prev (first coll)) (rest coll))
                  (gen (fn-prev-x prev (first coll)) (rest coll)))
              (conj acc prev)))]
    (when (seq coll)
      (gen () [(first coll)] (rest coll)))))

(defn combine-prev [prev x]
  (conj (pop prev) (+ x (* 10 (peek prev)))))

(defn neg-prev [prev x]
  (conj prev (- x)))

(defn eval-candidate [xs]
  (reduce + 0 xs))

(defn solutions []
  (let [negs #(expand-nums neg-prev %)
        nums #(expand-nums combine-prev %)
        hundred? #(= (eval-candidate %) 100)]
    (into () (comp (mapcat nums) (mapcat negs) (filter hundred?)) (list (range 1 10)))))


(defn print-solutions []
  (doseq [xs (solutions)]
    (print (first xs))
    (doseq [x (rest xs)]
      (if (neg? x)
        (print " -" (- x))
        (print " +" x)))
    (println " = 100")))



;;; END GOOD

;; Seems like there should be something built-in to Clojure to do the expand-nums stuff.
;; Maybe a transducer? With mapcat? Something with tree-seq?

(defn xnegs [coll]
  (expand-nums neg-prev coll))

(defn xnums [coll]
  (expand-nums combine-prev coll))

(defn xhundred? [coll]
  (= (eval-candidate coll) 100))

