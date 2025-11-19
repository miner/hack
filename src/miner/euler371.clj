(ns miner.euler371)

;;; NOT SOLVED HERE, but it's plausible to extrapolate from birthday problem.

;;; https://projecteuler.net/problem=371

;;; license plates "add" up to 1000 considering only the numbers
;;; What is the expected number of plates needed for a win?

;;; SEM: this is a probabilty question assuming equally likely digits.  Not about parsing!

;;; https://en.wikipedia.org/wiki/Birthday_problem

;;; Basically, needs math from birthday problem.  Slightly different in that match is actually
;;; (- 1000 x).  And "XXX-000" zero is unmatchable so that's a dead case, not 1/N
;;; probability.

;;; I don't like the "expected" terminology.  Is that 50%+ ?  Or 99.9999% ?


(def examples [["MIC-012" "HAN-988"] ["RYU-500" "SET-500"]])

(defn chdig [ch]
  (bit-and (long ch) 2r1111))

(defn pre-val [s]
  (reduce (fn [n c] (+ (* 10 n) (chdig c))) 0 s))

(defn plate-val [plate]
  (assert (and (string? plate) (= 7 (count plate))))
  (parse-long (subs plate 4)))

(defn pair-1000? [p1 p2]
  (= 1000   (+ (plate-val p1) (plate-val p2))))


;;; random 0 - 999

;;; simulate
(defn run-sim []
  (loop [seen #{}]
    (let [r (rand-int 1000)
          c (- 1000 r)]
      (if (contains? seen c)
        (inc (count seen))
        (recur (conj seen r))))))


(defn run-times-avg [n]
  (/ (reduce + 0 (repeatedly n run-sim)) (double n)))

; (run-times-avg 1000000)
;=> 39.689095

;; not really figured out yet:
;; the probability of a 1000 match is roughly 1/(1000 - #seen)
