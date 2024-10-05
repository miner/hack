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
  (let [resultv (brute-best-pins rv)
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




;;; FIXED in dpins -- the sorting of doubles isn't good enough as three conflicting doubles
;;; with larger in middle will never try just the two outer ones as the middle will dominate
;;; in a head to head test.  My conclusion is that you still need to do a search on double
;;; conflicts definitely good idea to start with all singles not sure they should be in one
;;; expanded reward, two vectors might be easier

;;; potential bug if two doubles are equal val, but are sensitive to the attempted
;;; insertion order -- say they were right next to each other.  Might have been better to do
;;; second one first!  Also, should restore single at rexp (- i 3) if you cancel double at (- i
;;; 2) -- but really that single is part of the calculation for [i d] -- not just d

;;; consider if there might be a cascade of double cancelations?  I don't think so.  We're
;;; taking the doubles in biggest score order.  But = d might miss???  Worse, there can be
;;; multiple conflicting doubles so you need search to resolve, including recovering singles.


(defn rand10 [n] (vec (repeatedly n #(- 10 (rand-int 20)))))


;;; debugging
(defn dconflicts [rv]
  (let [rexp (expand-reward rv)]
    (mapv #(max 0 %) (take-nth 2 rexp))))



;; double index i maps to pin i and i-1
(defn deconflict-double-hits [rv]
  (reduce (fn [dvs d]
            (into [] (mapcat (fn [dv]
                               (if-not (pos? d)
                                 [(conj dv 0)]
                                 (if (zero? (peek dv))
                                   [(conj dv d)]
                                   [(-> (pop dv) (conj 0) (conj d)) (conj dv 0)]))))
                  dvs))
          [[0]]
          (mapv * rv (subvec rv 1))))


;;; merging seems more expensive that it needs to be
;;; assumes first double is always zero so there's a peek, that's safe for now

;;; assume deconflicted dv so never two doubles in a row
;;; sv and dv should have zeroes instead of negatives
(defn merge-svdv1 [sv dv]
  (reduce-kv (fn [mv i d]
               (cond (zero? d) (conj (conj mv 0) (sv i))
                     (and (zero? (peek mv)) (> d (sv i))) (conj (conj mv d) 0)
                     (> d (+ (peek mv) (sv i))) (conj (conj (conj (pop mv) 0) d) 0)
                     :else (conj (conj mv 0) (sv i))))
             []
             dv))

(defn merge-svdv [sv dv]
  (reduce-kv (fn [mv i d]
               (cond (zero? d) (-> mv (conj 0) (conj (sv i)))
                     (and (zero? (peek mv)) (> d (sv i))) (-> mv (conj d) (conj 0))
                     (> d (+ (peek mv) (sv i))) (-> (pop mv) (conj 0) (conj d) (conj 0))
                     :else (-> mv (conj 0) (conj (sv i)))))
             []
             dv))


;;; 100x faster
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







;;; conjecture -- max decon-double is always best solution.  Mostly, but FAILS for a few
;;; cases so you can't use it!

;;; BUG xdpins  [2 3 -2 -8 -8 2 9 5 7 -3]

;;; buggy with some isses
;;; about 30% faster than dpins
(defn xdpins [rv]
  (if (empty? rv)
    [nil 0]
    (let [dvs (deconflict-double-hits rv)
          zv (mapv #(max 0 %) rv)
          best-dv (reduce (fn [bestv pv]
                            (let [score (reduce + 0 pv)]
                              (if (> score (peek bestv))
                                (conj pv score)
                                bestv)))
                               [-1]
                               dvs)
          bestv (merge-svdv zv (pop best-dv))]
      (conj bestv (reduce + 0 bestv)))))





;;; deconflict is doing full dv everytime.  It would be faster to make a tree with common
;;; heads so you only calc that once per fan out.

;;; deconflict is pretty good if there are only a few conflicts.  Not so great if there are
;;; lots of conflicts as with all positive rewards.

(defn decon-double-tree [rv]
  (reduce (fn [dvs d]
            (into [] (mapcat (fn [dv]
                               (if-not (pos? d)
                                 [(conj dv 0)]
                                 (if (zero? (peek dv))
                                   [(conj dv d)]
                                   [(-> (pop dv) (conj 0) (conj d)) (conj dv 0)]))))
                  dvs))
          [[0]]
          (mapv * rv (subvec rv 1))))



;; forget tree decon.  Try to integrate summing with deconfliction so you only continue on
;; greatest path.  NOT IMPLEMENTED



;;; FAILED EXPERIMENT -- tried to integrate operations of dpins.  not faster, not simpler
(defn decon [rv]
  (let [zv (mapv #(max 0 %) rv)]
    (reduce-kv (fn [dvs j d]
                 ;;(println "dvs" dvs j d)
                 (let [i (inc j)]
                   (into [] (mapcat (fn [dv]
                                      (cond (not (pos? d)) [(conj dv 0 (zv i))]
                                            (> d (+ (zv i) (zv (dec i))))
                                                (cond (and (zero? (peek dv))
                                                           (zero? (peek (pop dv))))
                                                          [(conj dv d 0) (conj dv 0 (zv i))]
                                                      (zero? (peek (pop dv)))
                                                          [(-> (pop dv) (conj 0) (conj d) (conj 0))
                                                           (conj dv 0 (zv i))]
                                                      :else
                                                          [(-> (pop (pop dv))
                                                               (conj 0)
                                                               (conj (zv (- i 2)))
                                                               (conj d)
                                                               (conj 0))
                                                           (conj dv 0 (zv i))])
                                            :else [(conj dv 0 (zv i))])))
                         dvs)))
               [[0 (zv 0)]]
               (mapv * rv (subvec rv 1)))))


;;; merging seems more expensive that it needs to be
;;; decided to only merge after picking best dv (after deconfliction) -- see xdpins

;;; assumes first double is always zero so there's a peek, that's safe for now

;;; but it's slower! works but not worth it.  Better idea in xdpins but it doesn't work!
(defn zpins [rv]
  (if (empty? rv)
    [nil 0]
    (reduce (fn [bestv pv]
              (let [score (reduce + 0 pv)]
                (if (> score (peek bestv))
                  (conj pv score)
                  bestv)))
            [-1]
            (decon rv))))




;;; UNIMPLEMENTED
;;; check d vs singles preconflict
;;; linear conflict zones instead of tree to control fanout


(defn conzone [rv]
  (reduce (fn [cv d]
            (let [p (peek cv)]
              (cond (vector? p) (if (not (pos? d)) (conj cv 0) (conj (pop cv) (conj p d)))
                    (not (pos? d)) (conj cv 0)
                    (zero? p) (conj cv d)
                    :else (conj (pop cv) [p d]))))
          [0]
          (mapv * rv (subvec rv 1))))

;;; that gives "zones of conflict" but still needs expansion for deconfliction

;;; deconfliction is basically take-nth 2 from two starts -- NO, you might win by skipping
;;; two intermediates to get on a better train -- you need to fanout

;;; cv should be all positives
(defn best-conflict [cv]
  (case (count cv)
    (0 1) cv
    2 (let [[a b] cv] (if (> a b) [a 0] [0 b]))
    (let [cvs (reduce (fn [cvs d]
                        (into [] (mapcat (fn [dv]
                                           (if (zero? d)
                                             [(conj dv 0)]
                                             (if (zero? (peek dv))
                                               [(conj dv d)]
                                               [(-> (pop dv) (conj 0) (conj d)) (conj dv 0)]))))
                              cvs))
                      ;; starting with zero simplifies logic for peeking
                      [[0]]
                      cv)]
      ;;(println "CVS" cvs)
      (-> (reduce (fn [bestcv cv]
                    (let [score (reduce + 0 cv)]
                      (if (> score (peek bestcv))
                        (conj cv score)
                        bestcv)))
                  [-1]
                  cvs)
          pop
          (subvec 1)))))


(defn bestcon [rv]
  (let [cz (reduce (fn [cv d]
                     (let [p (peek cv)]
                       (cond (vector? p) (if (not (pos? d))
                                           (into (pop cv) (conj (best-conflict p) 0))
                                           (conj (pop cv) (conj p d)))
                             (not (pos? d)) (conj cv 0)
                             (zero? p) (conj cv d)
                             :else (conj (pop cv) [p d]))))
                   [0]
                   (mapv * rv (subvec rv 1)))
        pz (peek cz)]
    (if (vector? pz)
      (into (pop cz) (best-conflict pz))
      cz)))


;;; BUGGY -- can't just pick best doubles
(defn bpins [rv]
  (if (empty? rv)
    [nil 0]
    (let [zv (mapv #(max 0 %) rv)
          bestv (merge-svdv zv (bestcon rv))]
      (conj bestv (reduce + 0 bestv)))))


(def bugs [[2 3 -2 -8 -8 2 9 5 7 -3] [7 2 8 1 -8 -5 8 -7 3 -6] [6 4 10 2 -8 10 8 3 -4 -3]])


(defn gentest
  ([] (gentest dpins 100))
  ([fpins] (gentest fpins 100))
  ([fpins n]
   (println (str fpins))
   (remove #(= (peek (brute-best-pins %)) (peek (fpins %)))
           (into bugs (repeatedly n #(rand10 10))))))


;;; best doubles doesn't work!  Most of the time it does but sometimes you can get enough
;;; singles to switch the balance


;;; WORKING:  brute-best-pins and dpins and zpins
;;; FAILING:  xdpins and bpins


