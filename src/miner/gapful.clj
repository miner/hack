(ns miner.gapful)

;; Eric's Challenge
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-361-tip-trampoline-your-tail-recursion/


;; gapful: integer has at least three digits and is divisible by the number formed by
;; concatenating the first and last digits.


(defn gapful? [n]
  (when (> n 99)
    (let [hi (loop [i n] (if (< i 10) i (recur (quot i 10))))
          lo (rem n 10)
          dd (+ lo (* 10 hi))]
      (zero? (rem n dd)))))

(defn nearest-gapful
  ([n] (if (gapful? n) n (nearest-gapful n 1)))
  ([n dist]
   (cond (gapful? (- n dist)) (- n dist)
         (gapful? (+ n dist)) (+ n dist)
         :else (recur n (inc dist)))))






;; difference should be +/- 1 for each step
(defn test-gap []
  (let [abs (fn [n] (if (neg? n) (- n) n))]
    (assert (reduce (fn [r x] (if (<= -1 (- r x) 1) x (reduced false)))
                    (map #(abs (- % (nearest-gapful2 %)))  (range 100 2345)))))
  true)












    
(defn rgap [n]
  (when (> n 99)
    (let [hi (first-digit n)
          lo (rem n 10)
          dd (+ lo (* 10 hi))]
      [n (rem n dd) (- n (nearest-gapful n))])))


(defn first-digit [n]
  (if (< n 10)
    n
    (recur (quot n 10))))


(defn gapful1? [n]
  (when (> n 99)
    (let [hi (first-digit n)
          lo (rem n 10)
          dd (+ lo (* 10 hi))]
      (zero? (rem n dd)))))


(defn mfdigit [n]
  (if (zero? n)
    0
    (quot n (long (Math/pow 10 (long (Math/log10 n)))))))

(defn fstr-digit [n]
  (- (long (first (str n))) (long \0)))

(defn digit [ch]
  (case ch
    \0 0
    \1 1
    \2 2
    \3 3
    \4 4
    \5 5
    \6 6
    \7 7
    \8 8
    \9 9
    nil))

(defn fdig [n]
  (digit (first (str n))))
