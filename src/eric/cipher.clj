(ns eric.cipher
  (:require [clojure.string :as str]))

;; https://gist.github.com/ericnormand/3154b336534ec8de911b0e99501584ab

;; Paul Cipher
;; 
;; Treat all letters as uppercase, and convert them to uppercase if needed.  The first
;; alphabetical character of the string will not change.  All subsequent alphabetical
;; characters are shifted toward Z by the alphabetical position of the preceding alphabetical
;; character.  Non-alphabetical characters are left as-is.

(defn alphanum [c]
  (case c
    (\A \a) 1 (\B \b) 2 (\C \c) 3 (\D \d) 4 (\E \e) 5 (\F \f) 6 (\G \g) 7 (\H \h) 8
    (\I \i) 9 (\J \j) 10 (\K \k) 11 (\L \l) 12 (\M \m) 13 (\N \n) 14 (\O \o) 15
    (\P \p) 16 (\Q \q) 17 (\R \r) 18 (\S \s) 19 (\T \t) 20 (\U \u) 21 (\V \v) 22
    (\W \w) 23 (\X \x) 24 (\Y \y) 25 (\Z \z) 26
    nil))

(defn alphanum2 [c]
  (let [c (int c)]
    (cond (<= (int \A) c (int \Z)) (- c (dec (int \A)))
          (<= (int \a) c (int \z)) (- c (dec (int \a)))
          :else nil)))
      
  
(defn rotn [c n]
  (let [d (int c)]
    (cond (<= (int \A) d (int \Z)) (char (+ (int \A) (mod (+ (- d (int \A)) n) 26)))
          (<= (int \a) d (int \z)) (char (+ (int \A) (mod (+ (- d (int \a)) n) 26)))
          :else c)))
    

(defn encode [s]
  (-> (reduce (fn [r c]
                (let [off (peek r)
                      alph (alphanum c)]
                  (if alph
                    (conj (pop r) (rotn c off) alph)
                    (conj (pop r) c off))))
              [0]
              s)
      pop
      str/join))


;; slightly faster???  Probably more conventional
(defn enc2 [s]
  (-> (reduce (fn [r c]
                (if-let [alph (alphanum c)]
                  (-> r
                      (update :res conj (rotn c (:offset r)))
                      (assoc :offset alph))
                  (update r :res conj c)))
              {:offset 0 :res []}
              s)
      :res
      str/join))




(defn decode [s]
  (-> (reduce (fn [r c]
                (let [off (peek r)
                      alph (alphanum c)]
                  (if alph
                    (let [ccc (rotn c off)]
                      (conj (pop r) ccc (- (alphanum ccc))))
                    (conj (pop r) c off))))
              [0]
              s)
      pop
      str/join))



(defn smoke-cipher [encode decode]
  (assert (= (encode "")  ""))
  (assert (= (encode "a")  "A"))
  (assert (= (encode "hello")  "HMQXA"))
  (assert (= (encode "newsletter") "NSBPEQYNYW"))
  (assert (= (encode "1 hug") "1 HCB"))
  (assert (= (decode "")  ""))
  (assert (= (decode "1") "1"))
  (assert (= (decode "HMQXA") "HELLO"))
  true)





