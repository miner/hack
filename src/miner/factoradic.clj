(ns miner.factoradic)

;;; Variable base number system.  Joke by XKCD.  But it actually works.
;;; https://xkcd.com/2835/

;;; Discussion:  https://explainxkcd.com/wiki/index.php/2835:_Factorial_Numbers


;;; Each power is a factorial of the previous places (from right)
;;; Decimal 7 == Fac 101
;;; Decimal 23 == Fac 321
;;; Decimal 5038 == Fac 654320

;;; from Right, digit power [1 2 6 24 120 720 ...]
;;; so Fac 201 = (+ (* 2 6) (* 0 2) (* 1 1)) = 13

;;; Note: the rightmost digit can be 0 or 1 only.  Many integer representations are not
;;; valid Factoradic numbers as digits have to be limited for the base power of that digit
;;; position.  In theory, you really would need extra digits (hex, etc.) for higher powers
;;; of Factoradics but XKCD just disallows those numbers to avoid the issue.

(def max-factoradic 987654321)

(defn max-fac-digit [place] (inc place))

;;; fac can be string of integer, but doesn't check for valid representation
(defn fac->dec [fac]
  (let [dv (mapv #(- (long %) (long \0)) (str fac))
        offset (inc (count dv))]
    (reduce-kv (fn [r i d] (+ d (* r (- offset i)))) 0 dv)))


;;; returns a long representing the factoradic.  Might be better to make it a string to
;;; avoid confusion with an actual long.  Not sure?  The loop is a fast (but not pretty) way
;;; to build up the factorial "base" divisors.  Somewhat faster to pass redundant params in
;;; loop than to (peek fs).

(defn dec->fac [dc]
  (first
   (reduce (fn [[fac r] p] [(+ (* fac 10) (quot r p)) (rem r p)])
           [0 dc]
           (loop [i 2 p 1 fs '(1)]
             (let [p2 (* p i)]
               (if (> p2 dc)
                 fs
                 (recur (inc i) p2 (conj fs p2))))))))



(defn df-test []
  (assert (= (dec->fac 0) 0))
  (assert (= (dec->fac 1) 1))
  (assert (= (dec->fac 2) 10))
  (assert (= (dec->fac 6) 100))
  (assert (= (fac->dec 101) 7))
  (assert (= (fac->dec 321) 23))
  (assert (= (fac->dec 654320) 5038))
  (assert (= 101 (dec->fac 7)))
  (assert (= 321 (dec->fac 23)))
  (assert (= 654320 (dec->fac 5038)))
  (assert (= (fac->dec (dec->fac 100000)) 100000))
  (assert (= (dec->fac 1000001) 266251221))
  true)



;;; other stuff


(defn chdig [c]
  (- (long c) (long \0)))

(defn valid-fac? [fac]
  (and (not (neg? fac))
       (<= fac max-factoradic)
       (reduce-kv (fn [_ i d] (if (> d (inc i)) (reduced false) true))
                  true
                  (mapv chdig (reverse (str fac))))))

;;; slower but cleaner
(defn valid-fac2? [fac]
  (and (not (neg? fac))
       (<= fac max-factoradic)
       (every? true? (map-indexed (fn [i d] (<= d (inc i)))
                                  (map chdig (reverse (str fac)))))))


(defn valid-fac3? [fac]
  (and (not (neg? fac))
       (<= fac max-factoradic)
       (every? true? (map-indexed (fn [i d] (<= (chdig d) (inc i)))
                                  (reverse (str fac))))))

(defn valid-fac4? [fac]
  (and (not (neg? fac))
       (<= fac max-factoradic)
       (transduce (comp (map chdig)
                       (map-indexed #(<= %2 (inc %))))
                 (fn ([_ x] (if x true (reduced false)))
                   ([x] x))
                 true
                 (reverse (str fac)))))

(defn valid-fac5? [fac]
  (and (not (neg? fac))
       (<= fac max-factoradic)
       (transduce (comp (map chdig)
                        (map-indexed #(<= %2 (inc %)))
                        (drop-while true?)
                        (take 1))
                  (completing conj empty?)
                  nil
                  (reverse (str fac)))))

(defn facpow [p]
  (reduce * (range 1 (inc p))))

(defn facpows [len]
  (reductions * (range 1 (inc len))))

;;; slower
(defn fac->dec0 [fac]
  (let [strfac (str fac)]
    (reduce + (map * (into nil (map chdig strfac)) (facpows (count strfac))))))

;;; shorter but slower
(defn fd2 [fac]
  (reduce + (map * (map chdig (reverse (str fac))) (reductions * (rest (range))))))

(defn fd3 [fac]
  (reduce + (map * (map chdig (seq (.reverse (StringBuilder. (str fac)))))
                 (reductions * (rest (range))))))



(defn high-pow [dc]
  (first (drop-while #(< % dc) (reductions * (iterate inc 1)))))

;;; zero-based place from the right
(defn high-place0 [dc]
  (ffirst (drop-while (fn [[i p]] (< p dc))
                      (map-indexed vector (reductions * (iterate inc 1))))))

;;; slower
(defn high-place2 [dc]
  (first (sequence (comp (map-indexed list)
                         (drop-while (fn [[i p]] (< p dc)))
                         (take 1)
                         (map first))
                   (reductions * (iterate inc 1)))))

;;; much faster but not needed anymore
(defn high-place [dc]
  (loop [i 1 v 1]
    (let [p (* v i)]
      (if (> p dc)
        (dec i)
        (recur (inc i) p)))))





;;; old version, later improved and integrated into dec->fac
(defn fplaces [dc]
  (loop [i 1 fs '(1)]
    (let [p (peek fs)]
      (if (> p dc)
        (pop fs)
        (recur (inc i) (conj fs (* p (inc i))))))))


;;; variation on dec->fac.  Returns long fac, not string.  Improvement
(defn dtof [dc]
  (first
   (reduce (fn [[fac r] p] [(+ (* fac 10) (quot r p)) (rem r p)])
           [0 dc]
           (fplaces dc))))




(defn dtofp [dc]
  (first
   (reduce (fn [[fac r] p] [(+ (* fac 10) (quot r p)) (rem r p)])
           [0 dc]
           (loop [i 1 fs '(1)]
             (let [p (peek fs)
                   p2 (* p (inc i))]
               (if (> p2 dc)
                 fs
                 (recur (inc i) (conj fs p2))))))))
           



(defn fpls [dc]
  (loop [i 1 fs '(1)]
    (let [p (peek fs)
          p2 (* p (inc i))]
      (if (> p2 dc)
        fs
        (recur (inc i) (conj fs p2))))))

;;; Doesn't check for negatives.   Buggy with calculated "digits" beyond 0-9.  That is,
;;; doesn't convert to hexadecimal or whatever for large digits.  XKCD says those
;;; factoradics are illegal.  :-)

;;; returns a string to avoid confusion with actual numbers
(defn dec->fac00 [dc]
  (if (zero? dc)
    "0"
    (->>
     ;; res is [places... remainder]
     (reduce (fn [res p]
               (let [r (peek res)]
                 (conj (pop res) (quot r p) (rem r p))))
             [dc]
             (fplaces dc))
     pop
     (apply str))))

