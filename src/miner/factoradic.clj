(ns miner.factoradic)

;;; Variable base number system.  Joke by XKCD.  But it actually works.
;;; https://xkcd.com/2835/

;;; Each power is a factorial of the previous places (from right)
;;; Decimal 7 == Fac 101
;;; Decimal 23 == Fac 321
;;; Decimal 5038 == Fac 654320

;;; from Right, digit power [1 2 6 24 120 720 ...]
;;; so Fac 201 = (+ (* 2 6) (* 0 2) (* 1 1)) = 13

;;; Note: the rightmost digit can be 0 or 1 only.  Many integer representations are not
;;; valid Factoradic numbers as digits have to be limited for the base power of that digit
;;; position.


(defn chdig [c]
  (- (long c) (long \0)))

(defn facpow [p]
  (reduce * (range 1 (inc p))))

(defn facpows [len]
  (reductions * (range 1 (inc len))))

;;; fac can be string of integer, but doesn't check for valid representation
(defn fac->dec [fac]
  (let [strfac (str fac)]
    (reduce + (map * (into nil (map chdig strfac)) (facpows (count strfac))))))

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

;;; much faster
(defn high-place [dc]
  (loop [i 1 v 1]
    (let [p (* v i)]
      (if (> p dc)
        (dec i)
        (recur (inc i) p)))))

(defn fplaces [dc]
  (loop [i 1 fs '(1)]
    (let [p (peek fs)]
      (if (> p dc)
        (pop fs)
        (recur (inc i) (conj fs (* p (inc i))))))))


;;; Doesn't check for negatives.

;;; returns a string to avoid confusion with actual numbers
(defn dec->fac [dc]
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

(defn df-test []
  (assert (= (dec->fac 0) "0"))
  (assert (= (dec->fac 1) "1"))
  (assert (= (dec->fac 2) "10"))
  (assert (= (dec->fac 6) "100"))
  (assert (= (fac->dec 101) 7))
  (assert (= (fac->dec 321) 23))
  (assert (= (fac->dec 654320) 5038))
  (assert (= "101" (dec->fac 7)))
  (assert (= "321" (dec->fac 23)))
  (assert (= "654320" (dec->fac 5038)))
  (assert (= (fac->dec (dec->fac 100000)) 100000))
  (assert (= (dec->fac 1000001) "266251221"))
  true)

