(ns advent21.adv14
  (:require [clojure.string :as str]))

;; part 2 shows combinatorial explosion so brute force failed.  Eventually, figured out I
;; had to cache frequencies from a reasonable size (20) and then use cache to add up to 40.
;; But that took some experimentation.  General solution might be to cache (or memoize)
;; several differnet levels and then make a plan that adds up correctly.  Didn't explore
;; that generalization.  Just did the 40=20x2 solution for part 2.

(defn parse-rules [input]
  (let [[pat _ & rulestrs] (str/split-lines input)]
    {:pattern pat
     :rules (reduce
             (fn [m rstr]
               (let [rstr (str/trim rstr)]
                 (assoc m (vector (first rstr) (second rstr)) (last rstr))))
             {}
             rulestrs)}))

;; rules are for insertions, so AB->C results in ACB.  Next time expands AC and CB, etc.

(def sample-rulestr
"NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")


(def prs (parse-rules sample-rulestr))


;; another idea:  compile rules to level N
;; CH -> B  really CH -> CBH -> CHBBHH -> CBH HCB BNB BHH HNH ->
;; works to size 20 but not practical at size 40.  But useful as a cache for "first" 20,
;; then use that cache to calc second 20 to get results for 40.

;; just apply step-rules to each rule head to precalc result in freqs
;; then apply to the partions of original pat
;; but that's more or less what xanswer2 was trying to do
;; maybe you need more intermediary partitions

(defn step-rules [rules pat]
  (into []
        (mapcat #(if-let [c (rules %)]
                   (list (first %) c)
                   (list (first %))))
        (partition-all 2 1 pat)))

(defn run-rules [n prules]
  (let [rules (:rules prules)]
    (loop [n n pat (:pattern prules)]
      (if (zero? n)
        (str/join pat)
        (recur (dec n) (step-rules rules pat))))))

(defn answer1 [n prules]
  (let [result (run-rules n prules)
        freq (frequencies result)
        cnts (sort (vals freq))]
    (- (last cnts) (first cnts))))


#_ (answer1 40 (parse-rules real-input))
;; Execution error (OutOfMemoryError) at advent21.adv14/step-rules$fn (adv14.clj:41).
;; GC overhead limit exceeded

;; New idea: work answer1 style on two char input -- drop last (original second char), get
;; freq and then merge  with next step of two.  That way you don't have to build a huge
;; temporary result before calculating the freqs.

;;; try to run first pair 40 times, queing letters, then 39 etc on the queue
;;; idea is to only have one growing pair at a time
;;; memoize pair full expansion fqs so you don't have to do it again
;;; maybe memoize pairs at 10, and do the expansion 4 times to get 40

;;; Even doing a two-char pattern 40 times is too much.  You have to do something to avoid
;;; the combinatorial explosion.  Try working the front and memoizing.  Or chunking
;;; memoizing.  Things at run 10 seem OK.  Actually up to 20 works OK.  Solution was to
;;; cache the rule head pairs for "first" 20 and use that cache on the expanded pattern
;;; (after 20) to add up "second" 20 frequencies.


;; my run-rules 20 is practical
;; cache results of all 20s per rule
;; re-run expanded 20 pattern, with cached

(defn experiment
  ([pat prules] (experiment 10 pat prules))
  ([ntimes pat prules]
  (dotimes [n ntimes]
    (let [res (run-rules n (assoc prules :pattern (seq pat)))]
      (println n (count res) (frequencies res))))))


;; slows down above 20, 25 is my max reasonable
(defn experkeys
  ([prules] (experkeys 10 prules))
  ([ntimes prules]
   (doseq [p (keys (:rules prules))]
     (println)
     (println p)
     (dotimes [n ntimes]
       (let [res (run-rules n (assoc prules :pattern (seq p)))]
         (println n (count res) (frequencies res)))))))


(defn inc1 [n]
  (if (nil? n) 1 (inc n)))



;; input is two char pat, but second is just for calc so we drop it at the end before
;; calculating the freqs "generated" by first char.
(defn freq-rules2 [n rules pat2]
  (if (= (count pat2) 1)
    (frequencies pat2)
    (loop [n n pat pat2]
      (if (zero? n)
        (frequencies (pop pat))
        (recur (dec n) (step-rules rules pat))))))

(defn answer2a [n prules]
  (let [rules (:rules prules)
        pat (:pattern prules)
        freqs (reduce (fn [fqs pat2] (merge-with + fqs (freq-rules2 n rules pat2)))
                      {}
                      (partition-all 2 1 pat))
        cnts (sort (vals freqs))]
    (- (last cnts) (first cnts))))


(defn xanswer2 [n prules]
  (let [rules (:rules prules)
        pat (:pattern prules)]
    (transduce (map #(freq-rules2 n rules %))
               (fn ([fqs fq] (merge-with + fqs fq))
                 ([fqs] (let [cnts (sort (vals fqs))]
                          (- (last cnts) (first cnts)))))
               {}
               (partition-all 2 1 pat))))





;; SOLUTION, PART 2

(defn xfqs2 [n pat2 rules]
  (transduce (map #(freq-rules2 n rules %))
               (fn ([fqs fq] (merge-with + fqs fq))
                 ([fqs] fqs))
               {}
               (partition-all 2 1 pat2)))

;; Kind of cheating, but 40 = 20x2 and conventional approach works at N=20 so cache that and
;; remap over expanded pattern for "second" 20.  Probably could have chunked at 10 and done
;; it multiple times, too.  This could use some more work, but it got the right answer so
;; I'm moving on.
(defn xans40 [prules]
  (let [rules (:rules prules)
        pat (:pattern prules)
        pat20 (loop [p pat n 20]
                (if (zero? n)
                  p
                  (recur (step-rules rules p) (dec n))))
        fq-cache (zipmap (keys rules)
                         (map #(update (xfqs2 20 % rules) (second %) dec)
                              (keys rules)))
        ;; cached 20, expanded pat 20 times, now use cache for "next" 20
        fqs (reduce (fn [fqs pair]
                      (merge-with + fqs (fq-cache pair (frequencies pair))))
                    {}
                    (partition-all 2 1 pat20))
        cnts (sort (vals fqs))]
    (- (last cnts) (first cnts))))
  


;; THIS DID NOT PAN OUT -- still too much stack.  Better to cache first 20.
;; depth first on first two, N times, then stack additions (for N-1, etc)
;; can't even step-rules for 40 with two char pat
;; could try depth first AB, AC+B, AD+C+B, AE+D+C+B
;;
;; 40 ABCD, 40 AB + 40 BCD, 39 AD + 39 DB + 40 BCD
;; 38 AE + 38 ED + 39 DB + 40 BCD
;; 37 AF + 37 FE + 38 ED + 39 DB + 40 BCD
;; 36 AG + 36 GF + 37 AF + 37 FE + 38 ED + 39 DB + 40 BCD
;; ... 0 AZ  + stack


;; but slower!

(defn ans2 [n prules]
  (let [rules (:rules prules)
        pat (:pattern prules)]
    (loop [stack (mapv #(list n %) (partition-all 2 1 pat))
           fqs {}]

      ;; DEBUG reverse looks better?
      ;;(println (rseq stack))

      (if (empty? stack)
        (let [cnts (sort (vals fqs))]
          (- (last cnts) (first cnts)))
        (let [[i pat] (peek stack)]
          (cond (= (count pat) 1) (recur (pop stack) (merge-with + fqs {(first pat) 1}))
                (zero? i) (recur (pop stack) (merge-with + fqs {(first pat) 1}))
                :else (if-let [match (rules pat)]
                                    (recur (conj (pop stack)
                                                 (list (dec i) (vector match (last pat)))
                                                 (list (dec i) (vector (first pat) match)))
                                           fqs)
                                    (recur (pop stack) (merge-with + fqs {(first pat)
                                                                          1})))))))))


;; faster than ans2, but still twice as slow as xanswer2
(defn ans3 [n prules]
  (let [rules (:rules prules)
        pat (:pattern prules)]
    (loop [work (map #(conj % n) (partition-all 2 1 pat))
           fqs {}]

      ;; DEBUG reverse looks better?
      ;;(println (rseq stack))

      (if (empty? work)
        (let [cnts (sort (vals fqs))]
          (- (last cnts) (first cnts)))
        (let [i (ffirst work)
              pat (rest (first work))]
          (cond (= (count pat) 1) (recur (rest work) (merge-with + fqs {(first pat) 1}))
                (zero? i) (recur (rest work) (merge-with + fqs {(first pat) 1}))
                :else (if-let [match (rules pat)]
                                    (recur (conj (rest work)
                                                 (list (dec i) match (last pat))
                                                 (list (dec i) (first pat) match))
                                           fqs)
                                    (recur (rest work)
                                           (merge-with + fqs {(first pat) 1})))))))))



                



(def real-input
"COPBCNPOBKCCFFBSVHKO

NS -> H
FS -> O
PO -> C
NV -> N
CK -> B
FK -> N
PS -> C
OF -> F
KK -> F
PP -> S
VS -> K
VB -> V
BP -> P
BB -> K
BF -> C
NN -> V
NO -> F
SV -> C
OK -> N
PH -> P
KV -> B
PN -> O
FN -> V
SK -> V
VC -> K
BH -> P
BO -> S
HS -> H
HK -> S
HC -> S
HF -> B
PC -> C
CF -> B
KN -> H
CS -> N
SP -> O
VH -> N
CC -> K
KP -> N
NP -> C
FO -> H
FV -> N
NC -> F
KB -> N
VP -> O
KO -> F
CP -> F
OH -> F
KC -> H
NB -> F
HO -> P
SC -> N
FF -> B
PB -> H
FB -> K
SN -> B
VO -> K
OO -> N
NF -> B
ON -> P
SF -> H
FP -> H
HV -> B
NH -> B
CO -> C
PV -> P
VV -> K
KS -> P
OS -> S
SB -> P
OC -> N
SO -> K
BS -> B
CH -> V
PK -> F
OB -> P
CN -> N
CB -> N
VF -> O
VN -> K
PF -> P
SH -> H
FH -> N
HP -> P
KF -> V
BK -> H
OP -> C
HH -> F
SS -> V
BN -> C
OV -> F
HB -> P
FC -> C
BV -> H
VK -> S
NK -> K
CV -> K
HN -> K
BC -> K
KH -> P")

(def rrs (parse-rules real-input))
