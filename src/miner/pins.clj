(ns miner.pins)

;; (:require [clojure.data.priority-map :as pm]) -- not needed

;;; Hacker News: Solving the bowling problem with dynamic programming
;;; https://news.ycombinator.com/item?id=41512129
;;; original   https://simonensemble.github.io/pluto_nbs/bowling.html

;;; problem from MIT 6.06
;;; https://youtu.be/r4-cftqTcdI?si=HIuQK9asc8sn4o8i&t=2220

;;; N bowling pins are arranged tightly along a line.  pin_i {1..N} labeled with
;;; reward r_i (int, could be negative)

;;; bowling ball can score r_i by hitting one pin directly, or hit between two pins, taking
;;; down both, and scoring multiple of (r_i * r_i+1)

;;; Take as many rolls as you want.  Leave some pins if you want.  How do you maximize
;;; accumulated score given reward vector.

;;; The article explains dynamic programming approach.  Probably more efficient than my
;;; brute force solution.

;;; For Clojure, we will use zero-based indexing

;;; Expanded reward interleaves the mutliples into the original reward vector.  The single
;;; pin rewards are at odd indices and the double hits (multiples) are at even indices.
;;; Always start with a 0 as there is no pin to multiply before the first one.  The negative
;;; rewards are never taken.  The goal then is to find possible choices.  We start with the
;;; choice from the left (index 0) and consider the choices two at a time (double or single
;;; rewards).  We can potentially skip, hit double or hit single.  We can always skip or
;;; single hit.  We can add a double hit only if the previous pin was a skip as we're trying
;;; to hit the previous pin and the current at the same time.

(def rewardv [3, 4, -1, 6, -1, 6, 6, 3, -1, -1, 6, -2])
;;; btwn mults [12 -4 -6  -6 -6 36 18 -3   1  -6 -12]
;;; expanded [0 3 12 4 -4 -1 -6 6 -6 -1 -6 6 36 6 18 3 -3 -1 1 -1 -6 6 -12 -2]

(def exx [3 4 -1 6])


;; Expanded reward: interleave the multiples. even indices are multiples, odds are single pin
;; rewards.  Always start with 0 as there is no double before first pin.

(defn expand-reward [rv]
  (reduce (fn [rexp r] (-> rexp (conj (* r (peek rexp))) (conj r)))
          [0 (rv 0)]
          (subvec rv 1)))


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
(defn brute-best-pins [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (expand-reward rv)
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
                        (if (pos? r1) [[0 0] [0 r1]] [[0 0]]))))))



;;; translate back into the notation used in the article (pins numbered 1..N and double hits as
;;; average of two pins
(defn report-pins [rv]
  (let [resultv (best-pins rv)
        score (peek resultv)]
    (println "; Reward" rv)
    (println "; Result" resultv)
    (println "; Score" score)
    (reduce-kv (fn [res i p]
                 (if (pos? p)
                   (let [pn (quot (inc i) 2)]
                     (conj res (if (odd? i) pn (+ 0.5 pn))))
                   res))
               []
               (pop resultv))))


;;; idea to be more efficient
;;; take all the singles, then try to fiddle for good doubles
;;; should be much smaller search space

(defn zneq [x] (if (neg? x) 0 x))



;;; BUG -- must clear both sides of double as we check randomly in value order

;;; still wonder about equal doubles?

;;; cases : no adjacent doubles, just compare two singles vs i double
;; BUG
;;; pv includes score, so we drops with pop
(defn replace-with-double-hit [rexp pv i d]
  (let [pv (pop pv) ;; drop existing score
        p1 (pv (dec i))
        p2 (pv (inc i))
        d0 (nth pv (- i 2) 0)
        d1 (nth pv (+ i 2) 0)]
    (cond (and (zero? d0) (zero? d1))  ;; no conflicting doubles
              (when (> d (+ p1 p2)) (assoc pv i d (inc i) 0 (dec i) 0))
          (and (pos? d0) (pos? d1)) ;; two adjacent conflicting doubles
              ;; I don't think this can happen when it would override
              (print "FIXME two conflict" i d pv)
          (pos? d0)  ;; only previous is conflicting
              (let [s0 (nth rexp (- i 3) 0)
                    si (nth rexp (inc i) 0)]
                (when (> (+ d s0) (+ d0 si))
                  (assoc pv i d (inc i) 0 (dec i) 0 (- i 2) 0 (- i 3) s0)))
          (pos? d1)  ;; only trailing is conflicting
              (let [s1 (nth rexp (+ i 3) 0)]
                (when (> (+ d s1) d1)
                  (assoc pv i d (inc i) 0 (dec i) 0 (+ i 2) 0 (+ i 3) s1)))
          )))


;;; FIXME the sorting of doubles isn't good enough as three conflicting doubles with larger
;;; in middle will never try just the two outer ones as the middle will dominate in a head
;;; to head test.  My conclusion is that you still need to do a search on double conflicts
;;; definitely good idea to start with all singles
;;; not sure they should be in one expanded reward, two vectors might be easier


;;; potential bug if two doubles are equal val, but are sensitive to the attempted
;;; insertion order -- say they were right next to each other.  Might have been better to do
;;; second one first!  Also, should restore single at rexp (- i 3) if you cancel double at (- i
;;; 2) -- but really that single is part of the calculation for [i d] -- not just d

;;; consider if there might be a cascade of double cancelations?  I don't think so.  We're
;;; taking the doubles in biggest score order.  But = d might miss???  Worse, there can be
;;; multiple conflicting doubles so you need search to resolve, including recovering singles.

;;; much faster

(defn heurpins [rv]
  (if (empty? rv)
    [nil 0]
    (let [rexp (expand-reward rv)
          sv (reduce-kv (fn [sv i x] (if (or (even? i) (neg? x)) (conj sv 0) (conj sv x)))
                        []
                        rexp)
          md (reduce (fn [md i] (let [x (rexp i)] (if (pos? x) (assoc md i x) md)))
                     {}
                     (range 2 (count rexp) 2))]
      ;; (rexp 0) is always zero so we can skip it

      ;; (println "rexp" rexp)
      ;; (println "sv  " sv)
      ;; (println "md  " md)
      (reduce-kv (fn [pv i d]
                   ;; (println "hp" i d pv)
                   (let [score (peek pv)
                         dv (replace-with-double-hit rexp pv i d)
                         dscore (reduce + dv)]
                     (if (> dscore score)
                       (conj dv dscore)
                       pv)))
                 (conj sv (reduce + sv))
                 (sort-by val #(compare %2 %) md)))))



(defn rand10 [n] (vec (repeatedly n #(- 10 (rand-int 20)))))

(defn gentest []
  (remove #(= (brute-best-pins %) (heurpins %)) (repeatedly 100 #(rand10 10))))

(def fails [[-7 -6 5 3 7 2 -3 4 0 10] [-1 -2 4 6 6 6 1 5 10 2] [1 7 2 -2 -8 -3 -6 3 -9 0]
            [0 7 8 10 4 -2 -1 -4 4 -9] [7 -8 0 -3 -9 -3 5 2 -9 2] [-1 -8 0 0 6 9 4 9 6 5]
            [-6 1 8 3 -3 -9 -3 4 2 2]])

(defn dconflicts [rv]
  (let [rexp (expand-reward rv)]
    (mapv #(max 0 %) (take-nth 2 rexp))))



[0 42 0 15 21 14 0 0 0 0]

[0 42 0 15 0 14 0 0 0 0]
[0 42 0  0 21 0 0 0 0 0]

;; double index i maps to pin i and i-1
(defn deconflict-double-hits [rv]
  (let [rv1 (subvec rv 1)
        dv (mapv * rv rv1)]
    (reduce (fn [dvs d]
              (into [] (mapcat (fn [dv]
                                 (if-not (pos? d)
                                   [(conj dv 0)]
                                   (if (zero? (peek dv))
                                     [(conj dv d)]
                                     [(conj (pop dv) 0 d) (conj dv 0)]))))
                    dvs))
            [[0]]
            dv)))


;;; assume deconflicted dv so never two doubles in a row
(defn merge-svdv [sv dv]
  (reduce-kv (fn [mv i d]
               (cond (zero? d) (conj mv 0 (sv i))
                     (and (zero? (peek mv)) (> d (sv i))) (conj mv d 0)
                     (> d (+ (peek mv) (sv i))) (conj (pop mv) 0 d 0)
                     :else (conj mv (sv i))))
             []
             dv))

;;; seems to work now
(defn dpins [rv]
  (if (empty? rv)
    [nil 0]
    (let [dvs (deconflict-double-hits rv)
          zv (mapv #(max 0 %) rv)]
      (reduce (fn [bestv pv]
                (let [score (reduce + 0 pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (mapv #(merge-svdv zv %) dvs)))))



(defn dgentest []
  (remove #(= (peek (brute-best-pins %)) (peek (dpins %))) (repeatedly 100 #(rand10 10))))
