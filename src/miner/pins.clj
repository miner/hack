(ns miner.pins)

;;  (:require [clojure.data.priority-map :as pm]) -- not needed

;;; Hacker News: Solving the bowling problem with dynamic programming
;;; https://news.ycombinator.com/item?id=41512129
;;; original   https://simonensemble.github.io/pluto_nbs/bowling.html

;;; problem from MIT 6.06
;;; https://youtu.be/r4-cftqTcdI?si=HIuQK9asc8sn4o8i&t=2220

;;; N bowling pins are arranged tightly along a line.  pin_i {1..N} labeled with
;;; reward r_i (int, could be negative)

;;; bowling ball can score r_i by hitting one pin directly, or hit between two pins, taking
;;; down both, and scoring multiple of (r_i * r_i+1)

;;; Take as many roles as you want.  Leave some pins if you want.  How do you maximize
;;; accumulated score given reward vector.

;;; The article explains dynamic programming approach.  Probably more efficient than my
;;; brute force solution.

;;; For Clojure, we will use zero-based indexing

;;; Expanded reward interleaves the mutliples into the original reward vector.  The single
;;; pin rewards are at odd indices and the double hits (multiples) are at even indices.
;;; Always start with a 0 as there is no pin to multiply before the first one.  The negative
;;; rewards are never taken so we zero them out.  The goal then is to find possible choices.
;;; We start with the choice from the left (index 0) and consider the choices two at a
;;; time (double or single rewards).  We can potentially skip, hit double or hit single.
;;; We can always skip or single hit.  We can add a double hit only if the previous pin was
;;; a skip as we're trying to hit the previous pin and the current at the same time.

(def rewardv [3, 4, -1, 6, -1, 6, 6, 3, -1, -1, 6, -2])
;;; btwn mults [12 -4 -6  -6 -6 36 18 -3   1  -6 -12]
;;; expanded [0 3 12 4 -4 -1 -6 6 -6 -1 -6 6 36 6 18 3 -3 -1 1 -1 -6 6 -12 -2]

(def exx [3 4 -1 6])



(defn zneg [^long x]
  (if (neg? x) 0 x))

;; Expanded reward: interleave the multiples. even indices are multiples, odds are single pin
;; rewards.  Always start with 0 as there is no double before first pin.

(defn expand-zero-reward [rv]
  (mapv zneg
        (reduce (fn [rexp r] (-> rexp (conj (* r (peek rexp))) (conj r)))
                [0 (rv 0)]
                (subvec rv 1))))


;;; three possible extensions
;;; always next skip
;;; always next single
;;; next double only if previous was skip
;;; but don't add if the item is a zero (skip is already there)

;; every extension must be two additional elements, double first, single second
;; skip is 0 0 addition, which is always added
;; avoid redundant zeros (if the reward is already 0, don't add again)
(defn extend-pinv [rexp pv]
  (let [nd (rexp (count pv))
        n1 (rexp (inc (count pv)))
        pv0 (conj pv 0)]
    (cond-> [(conj pv0 0)]
      (pos? n1)  (conj (conj pv0 n1))
      (and (pos? nd) (zero? (peek pv)) (zero? (peek (pop pv))))  (conj (-> pv (conj nd) (conj 0))))))




;; calls (f init) then f on the nested result `n` times.  Zero-th is just init.
(defn iterated [n f init]
  (if (pos? n)
    (recur (unchecked-dec n) f (f init))
    init))

;; returns vector of expanded rewards that are chosen, plus the total score
(defn best-pins [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (expand-zero-reward rv)
          extend2 (fn [pvs] (into [] (mapcat #(extend-pinv rexp %)) pvs))
          ;; (rexp 0) is always 0 for convenience
          r1 (rexp 1)]
      (reduce (fn [bestv pv]
                (let [score (reduce + 0 pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (iterated (dec (count rv))
                        extend2
                        (if (zero? r1) [[0 0]] [[0 0] [0 r1]]))))))


;;; for debugging
(defn calc-all [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (zzreward rv)
          xexpander (mapcat #(expand-pinv rexp %))
          r1 (rexp 1)
          results (iterated (dec (count rv))
                            (fn [pvs] (into [] xexpander pvs))
                            (if (zero? r1) [[0 0]] [[0 0] [0 r1]]))]
      (println "calc top 10 of" (count results))
      (take 10
            (sort-by (comp - peek)
                     (into [] (map #(conj % (reduce + %))) results))))))


;;; translate back into the notation used in the article (pins numbered 1..N and double hits as
;;; average of two pins
(defn report-pins [rv]
  (let [resultv (best-way rv)
        score (peek resultv)]
    (println "; Result" resultv)
    (println "; Score" score)
    (reduce-kv (fn [res i p]
                 (if (pos? p)
                   (let [pn (quot (inc i) 2)]
                     (conj res (if (odd? i) pn (+ 0.5 pn))))
                   res))
               []
               (pop resultv))))

