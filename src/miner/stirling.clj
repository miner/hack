(ns miner.stirling
  (:require [clojure.math :as m]))

;;; https://en.wikipedia.org//wiki/Stirling's_approximation
;;; approximation for factorial

(defn stirling [n]
  (if (zero? n)
    1.0
    (m/rint (* (m/sqrt (* 2.0 m/PI n))
               (m/pow (/ n m/E) n)))))


(defn fact [n]
  (reduce *' (range 2 (inc n))))


;; error is less than 1% for n > 10
(defn estir [n]
  (let [f (fact n)]
    (/ (- f (stirling n)) f)))
       

;; hypbrid approach, fast enough, simplified stirling for higher N
(defn factstir [n]
  (if (> n 20)
    (* (m/sqrt (* 2.0 m/PI n))
       (m/pow (/ n m/E) n))
    (reduce * (range 2 (inc n)))))

