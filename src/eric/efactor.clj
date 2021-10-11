(ns miner.efactor
  (:require [clojure.string :as str]))

;; Eric Challenge: stringify prime factorization given factors

;; https://gist.github.com/ericnormand/087eab23272b3ed0d7a8e3007b699a1d

#_
(require '[clojure.string :as str])

;; Works with unordered factors
;; faster with transducers
(defn factors->string [factors]
  (str (reduce * 1 factors) " = "
       (str/join " x "  (sequence (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                                  (sort (frequencies factors))))))


(defn sifact [factors]
  (apply str (reduce * 1 factors) " = "
         (sequence (comp (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt))))
                         (interpose " x "))
                   (sort (frequencies factors)))))




;; sequence version
(defn strfact [factors]
  (let [fcnts (frequencies factors)]
    (apply str (reduce * 1 factors) " = "
           (interpose " x " (map (fn [[f cnt]] (if (= cnt 1) f (str f "^" cnt)))
                                 (sort fcnts))))))



;; assuming factors is vector in order, but not faster
(defn pbfact [factors]
  (apply str (reduce * 1 factors) " = "
         (sequence (comp (partition-by identity)
                         (map (fn [fs] (let [cnt (count fs)]
                                         (if (= cnt 1)
                                           (peek fs)
                                           (str (peek fs) "^" cnt)))))
                         (interpose " x ") )
                   factors)))




(defn smoke-fact [factors->string]
  (assert (= (factors->string [2 2 2 3]) "24 = 2^3 x 3"))
  (assert (= (factors->string [7]) "7 = 7"))
  (assert (= (factors->string [2 2 7]) "28 = 2^2 x 7"))
  true)

