(ns miner.geometry)

(defn sqrt [x]
  (Math/sqrt (double x)))

(defn sq [x]
  (* x x))

(defn hyp [a b]
  (sqrt (+ (sq a) (sq b))))


(defn frac
  "Fractional part.  Could be negative if x is negative."
  [x]
  (rem x 1.0))

(def whole long)

(defn myfrac [x]
  (- (double x) (long x)))






(let [seq-of-seqs [(range  0  5)
                     (range 10 15)
                   (range 20 25)]
      conc-seq (apply concat seq-of-seqs)
      seq-seq (sequence cat seq-of-seqs)]
  (= (range 25)
     conc-seq
     seq-seq))
