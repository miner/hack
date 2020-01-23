(ns miner.gapful)

;; Eric's Challenge
;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-361-tip-trampoline-your-tail-recursion/


;; gapful: integer has at least three digits and is divisible by the number formed by
;; concatenating the first and last digits.


(defn first-digit [n]
  (if (< n 10)
    n
    (recur (quot n 10))))

  
(defn gapful? [n]
  (when (> n 99)
    (let [d0 (first-digit n)
          dn (rem n 10)
          dd (+ dn (* 10 d0))]
      (zero? (rem n dd)))))


(defn nearest-gapful
  ([n] (if (gapful? n) n (nearest-gapful n 1)))
  ([n dist]
   (cond (gapful? (- n dist)) (- n dist)
         (gapful? (+ n dist)) (+ n dist)
         :else (recur n (inc dist)))))
    


(defn abs [n] (if (neg? n) (- n) n))

(defn test-gap []
  (assert (reduce (fn [r x] (if (<= -1 (- r x) 1) (abs x) (reduced nil)))
                  (map #(abs (- % (nearest-gapful %)))  (range 100 2345))))
  true)



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
