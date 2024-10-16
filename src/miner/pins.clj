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

(defn zneg [x] (if (neg? x) 0 x))

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
      (and (pos? nd) (zero? (peek pv)) (zero? (peek (pop pv))))
        (conj (-> pv (conj nd) (conj 0))))))




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

(defn zbase1 [pinv]
  (reduce-kv (fn [res i p]
                 (if (pos? p)
                   (let [pn (quot (inc i) 2)]
                     (conj res (if (odd? i) pn (+ 0.5 pn))))
                   res))
               []
               (pop pinv)))
  
(defn report-pins
  ([] (report-pins rewardv))
  ([rv] (report-pins brute-best-pins rv))
  ([pin-fn rv]
   (let [resultv (pin-fn rv)
         score (peek resultv)]
     (println ";" (str pin-fn))
     (println "; Reward" rv "  count =" (count rv))
     (println "; Result" resultv)
     (println "; Score" score)
     (zbase1 resultv))))


;;; idea to be more efficient
;;; take all the singles, then try to fiddle for good doubles
;;; should be much smaller search space





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

;;; by far the best approach is rpins near the end of file.  Need to keep two running totals
;;; to decide best path.
;;; lots of slower ideas explored below


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

(defn merge-svdv [sv dv]
  (reduce-kv (fn [mv i d]
               (cond (zero? d) (-> mv (conj 0) (conj (sv i)))
                     (and (zero? (peek mv)) (> d (sv i))) (-> mv (conj d) (conj 0))
                     (> d (+ (peek mv) (sv i))) (-> (pop mv) (conj 0) (conj d) (conj 0))
                     :else (-> mv (conj 0) (conj (sv i)))))
             []
             dv))


;;; 100x faster than brute but much slower than rpins at the end of file
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


;; I like the transducer version -- basically the same
(defn xdpins [rv]
  (if (empty? rv)
    [nil 0]
    (let [zv (mapv #(max 0 %) rv)]
      (transduce (map #(merge-svdv zv %))
                 (fn ([bestv] bestv)
                   ([bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv))))
                 [-1]
                 (deconflict-double-hits rv)))))




(defn xdpins1 [rv]
  (if (empty? rv)
    [nil 0]
    (let [sv (mapv zneg rv)
          merge-sd (fn [dv]
                     (reduce-kv (fn [mv i d]
                                  (cond (zero? d) (-> mv (conj 0) (conj (sv i)))
                                        (and (zero? (peek mv)) (> d (sv i)))
                                            (-> mv (conj d) (conj 0))
                                        (> d (+ (peek mv) (sv i)))
                                            (-> (pop mv) (conj 0) (conj d) (conj 0))
                                        :else (-> mv (conj 0) (conj (sv i)))))
                                []
                                dv))
          decond (fn [rv]
                   (reduce (fn [dvs d]
                             (into [] (mapcat (fn [dv]
                                                (if-not (pos? d)
                                                  [(conj dv 0)]
                                                  (if (zero? (peek dv))
                                                    [(conj dv d)]
                                                    [(-> (pop dv) (conj 0) (conj d))
                                                     (conj dv 0)]))))
                                   dvs))
                           [[0]]
                           (mapv * rv (subvec rv 1))))]
      (transduce (map merge-sd)
                 (fn ([bestv] bestv)
                   ([bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv))))
                 [-1]
                 (decond rv)))))

;; faster
(defn xdpins2 [rv]
  (if (empty? rv)
    [nil 0]
    (let [sv (mapv zneg rv)
          merge-sd (fn [dv]
                     (reduce-kv (fn [mv i d]
                                  (cond (zero? d) (-> mv (conj 0) (conj (sv i)))
                                        (> d (+ (peek mv) (sv i)))
                                            (-> (pop mv) (conj 0) (conj d) (conj 0))
                                        :else (-> mv (conj 0) (conj (sv i)))))
                                []
                                dv))
          decond (fn [rv]
                   (reduce (fn [dvs d]
                             (into []
                                   (mapcat (fn [dv]
                                             (cond (not (pos? d)) [(conj dv 0)]
                                                   (zero? (peek dv)) [(conj dv d)]
                                                   (let [i (count dv)]
                                                     (<= d (+ (sv i) (sv (dec i)))))
                                                       [(conj dv 0)]
                                                   :else [(-> (pop dv) (conj 0) (conj d))
                                                          (conj dv 0)])))
                                   dvs))
                           [[0]]
                           (mapv * rv (subvec rv 1))))]
      (transduce (map merge-sd)
                 (fn ([bestv] bestv)
                   ([bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv))))
                 [-1]
                 (decond rv)))))


;;; same, not worth it
(defn xdpins21 [rv]
  (if (empty? rv)
    [nil 0]
    (let [sv (mapv zneg rv)
          expd (fn [dv d]
                 (cond (not (pos? d)) [(conj dv 0)]
                       (zero? (peek dv)) [(conj dv d)]
                       (let [i (count dv)]
                         (<= d (+ (sv i) (sv (dec i)))))
                           [(conj dv 0)]
                       :else [(-> (pop dv) (conj 0) (conj d))
                              (conj dv 0)]))
          decond (fn [rv] (reduce (fn [dvs d] (into [] (mapcat #(expd % d)) dvs))
                                  [[0]]
                                  (mapv * rv (subvec rv 1))))
          merge-sd (fn [dv]
                     (reduce-kv (fn [mv i d]
                                  (cond (zero? d) (-> mv (conj 0) (conj (sv i)))
                                        (> d (+ (peek mv) (sv i)))
                                            (-> (pop mv) (conj 0) (conj d) (conj 0))
                                        :else (-> mv (conj 0) (conj (sv i)))))
                                []
                                dv))]
      (transduce (map merge-sd)
                 (fn ([bestv] bestv)
                   ([bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv))))
                 [-1]
                 (decond rv)))))




;; slower slightly
(defn xdpins3 [rv]
  (if (empty? rv)
    [nil 0]
    (let [sv (mapv zneg rv)
          merge-sd (fn [dv]
                     (reduce-kv (fn [mv i d]
                                  (cond (zero? d) (-> mv (conj 0) (conj (sv i)))
                                        (> d (+ (peek mv) (sv i)))
                                           (-> (pop mv) (conj 0) (conj d) (conj 0))
                                        :else (-> mv (conj 0) (conj (sv i)))))
                                []
                                dv))
          decond (fn [rv]
                   (reduce (fn [dvs d]
                             (into []
                                   (mapcat (fn [dv]
                                             (cond (not (pos? d)) [(conj dv 0)]
                                                   (zero? (peek dv)) [(conj dv d)]
                                                   #_ (let [i (count dv)]
                                                     (<= d (+ (sv i) (sv (dec i)))))
                                                   #_   [(conj dv 0)]
                                                   :else [(-> (pop dv) (conj 0) (conj d))
                                                          (conj dv 0)])))
                                   dvs))
                           [[0]]
                           (mapv * rv (subvec rv 1))))]
      (transduce (map merge-sd)
                 (fn ([bestv] bestv)
                   ([bestv pv]
                    (let [score (reduce + 0 pv)]
                      (if (> score (peek bestv))
                        (conj pv score)
                        bestv))))
                 [-1]
                 (decond rv)))))






;;; adds extra zero fan out if prev d is 0 -- makes easier merge but much slower overall
(defn decon-hits [rv]
  (reduce (fn [dvs d]
            (into [] (mapcat (fn [dv]
                               (if-not (pos? d)
                                 [(conj dv 0)]
                                 (if (zero? (peek dv))
                                   ;; extra zero choice
                                   [(conj dv d) (conj dv 0)]
                                   [(-> (pop dv) (conj 0) (conj d)) (conj dv 0)]))))
                  dvs))
          [[0]]
          (mapv * rv (subvec rv 1))))



(defn merge-rvdv [rv dv]
  (reduce-kv (fn [mv i s]
               (if (and (zero? (dv i)) (zero? (nth dv (inc i) 0)))
                 (-> mv (conj 0) (conj (max 0 s)))
                 (-> mv (conj (dv i)) (conj 0))))
             []
             rv))


(defn dpins0 [rv]
  (if (empty? rv)
    [nil 0]
      (reduce (fn [bestv pv]
                (let [score (reduce + 0 pv)]
                  (if (> score (peek bestv))
                    (conj pv score)
                    bestv)))
              [-1]
              (mapv #(merge-rvdv rv %) (decon-hits rv)))))



(defn dpins0x [rv]
  (if (empty? rv)
    [nil 0]
    (transduce (map #(merge-rvdv rv %))
               (fn ([bestv] bestv)
                 ([bestv pv]
                  (let [score (reduce + 0 pv)]
                    (if (> score (peek bestv))
                      (conj pv score)
                      bestv))))
               [-1]
               (decon-hits rv))))




;;; conjecture -- max decon-double is always best solution.  Mostly, but FAILS for a few
;;; cases so you can't use it!

;;; BUG pins  [2 3 -2 -8 -8 2 9 5 7 -3]



;;; not fully explored but seems like it's not going to win
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




(def bugs [[2 3 -2 -8 -8 2 9 5 7 -3] [7 2 8 1 -8 -5 8 -7 3 -6] [6 4 10 2 -8 10 8 3 -4 -3]
           [-5 2 0 1 -4 4 -2 -6 -5 -8]])

(defn fname [f]
  (let [r (re-find #"[$].*[@]" (str f))]
    (subs r 1 (dec (count r)))))


(defn report-bugs
  ([fpins] (report-bugs fpins bugs))
  ([fpins bugs]
   (println (fname fpins))
   (if (empty? bugs)
     true
     (clojure.pprint/pprint (interleave bugs (mapv brute-best-pins bugs) (mapv fpins bugs))))))


(defn gentest
  ([] (gentest dpins 100))
  ([fpins] (gentest fpins 100))
  ([fpins n]
   (println (fname fpins))
   (remove #(= (peek (brute-best-pins %)) (peek (fpins %)))
           (into bugs (repeatedly n #(rand10 10))))))


;;; Note: there maybe multiple good solutions so = is not always a good test
(defn gentest=
  ([] (gentest= dpins 100))
  ([fpins] (gentest= fpins 100))
  ([fpins n]
   (println "checking=")
   (report-bugs fpins (take 10 (remove #(= (brute-best-pins %) (fpins %))
                                       (into bugs (repeatedly n #(rand10 10))))))))


;;; HN comment
;   values = [3, 4, -1, 6, -1, 6, 6, 3, -1, -1, 6, -2]
;   prev_val = prev_score = score = 0
;   for val in values:
;     prev_val, prev_score, score = val, score, max(score, score + val, prev_score + val * prev_val)
;   print(score)  # 64


;;; only returns the final score, not the pins chosen

(defn score-pins [rv]
  (peek (reduce (fn [[pval pscore score] r]
                  [r score (max score (+ score r) (+ pscore (* r pval)))])
                [0 0 0]
                rv)))



;; res is the vector of alternating double-hit / single-hit values chosen for the highest
;; score.  Same style as my slower experiments.



;;; Much faster -- big winner
;;; returns same format as my other versions

;;; new fastest
;;; a bit faster to reduce-conj rather than use larger arity conj or into
;;; a bit faster to pass pval rather than lookup with reduce-kv (nth rv (dec i) 0)
;;; my theory is any conditional introduction slows down the pipeline
(defn rpins [rv]
  (let [[score res]
        (reduce (fn [[score res pscore pres pval] r]
                     (let [wsing (+ score r)
                           d (* r pval)
                           wdoub (+ pscore d)]
                       (cond (and (>= score wsing) (>= score wdoub))
                                 [score (reduce conj res [0 0]) score res r]
                             (>= wsing wdoub)
                                 [wsing (reduce conj res [0 r]) score res r]
                             ;; take wdoub
                             :else [wdoub (reduce conj pres [0 0 d 0]) score res r])))
                   [0 [] 0 [] 0]
                   rv)]
    (conj res score)))



;;; Try combining score and res into same vector -- more compact and nicer but slower to do
;;; extra poppping.

(defn irpins [rv]
  (peek
   (reduce-kv (fn [[pres res] i r]
                (let [score (peek res)
                      pscore (peek pres)
                      wsing (+ score r)
                      d (* r (nth rv (dec i) 0))
                      wdoub (+ pscore d)]
                  (cond (and (>= score wsing) (>= score wdoub))
                            [res (reduce conj (pop res) [0 0 score])]
                        (>= wsing wdoub)
                            [res (reduce conj (pop res) [0 r wsing])]
                        ;; take wdoub
                        :else [res (reduce conj (pop pres) [0 0 d 0 wdoub])])))
              [[0] [0]]
              rv)))


;;; a different notation could use (doub) as a list, keeping the pin i the same as rewardv
;;; [0 (4) 0 3 0 (16) 1 14]


