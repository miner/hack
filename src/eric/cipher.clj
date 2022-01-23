(ns eric.cipher
  (:require [clojure.string :as str]))

;; https://gist.github.com/ericnormand/3154b336534ec8de911b0e99501584ab

;; Paul Cipher
;; 
;; Treat all letters as uppercase, and convert them to uppercase if needed.  The first
;; alphabetical character of the string will not change.  All subsequent alphabetical
;; characters are shifted toward Z by the alphabetical position of the preceding alphabetical
;; character.  Non-alphabetical characters are left as-is.


(defn alnum [c]
  (case c
    \A 1 \B 2 \C 3 \D 4 \E 5 \F 6 \G 7 \H 8 \I 9 \J 10
    \K 11 \L 12 \M 13 \N 14 \O 15 \P 16 \Q 17 \R 18 \S 19 \T 20
    \U 21 \V 22 \W 23 \X 24 \Y 25 \Z 26
    nil))

;; assumes c is A-Z
(defn rotaten [c n]
  (char (+ (int \A) (mod (+ (- (int c) (int \A)) n) 26))))

(defn encode [s]
  (-> (reduce (fn [r c]
                (if-let [alph (alnum c)]
                  (conj (pop r) (rotaten c (peek r)) alph)
                  (conj (pop r) c (peek r))))
              [0]
              (clojure.string/upper-case s))
      pop
      clojure.string/join))

(defn decode [s]
  (-> (reduce (fn [r c]
                (if (alnum c)
                  (let [cc (rotaten c (peek r))]
                    (conj (pop r) cc (- (alnum cc))))
                  (conj (pop r) c (peek r))))
              [0]
              (clojure.string/upper-case s))
      pop
      clojure.string/join))


;; inspired by @jonasseglare -- mutation but fast
;; [My reduce version was slower so I deleted it]
;; just as fast to use .length as it is to count i with inc

(defn encode4 [^String s]
  (let [len (.length s)
        s (.toUpperCase s)]
    (loop [sb (StringBuilder. ^int len) rot 0]
      (let [i (.length sb)]
        (if (>= i len)
          (.toString sb)
          (let [c (.charAt s i)]
            (if-let [alp (alnum c)]
              (recur (.append sb ^char (rotaten c rot)) (long alp))
              (recur (.append sb ^char c) rot))))))))

(defn decode4 [^String s]
  (let [len (.length s)
        s (.toUpperCase s)]
    (loop [sb (StringBuilder. ^int len) rot 0]
      (let [i (.length sb)]
        (if (>= i len)
          (.toString sb)
          (let [c (.charAt s i)]
            (if-let [cc (when (alnum c) (rotaten c rot))]
              (recur (.append sb ^char cc) (long (- (alnum cc))))
              (recur (.append sb ^char c) rot))))))))




(defn smoke-cipher [encode decode]
  (assert (= (encode "")  ""))
  (assert (= (encode "a")  "A"))
  (assert (= (encode "hello")  "HMQXA"))
  (assert (= (encode "newsletter") "NSBPEQYNYW"))
  (assert (= (encode "1 hug") "1 HCB"))
  (assert (= (decode "")  ""))
  (assert (= (decode "1") "1"))
  (assert (= (decode "HMQXA") "HELLO"))
  (let [msg "GO 4 IT BE4 IT'S 2 LATE"]
    (assert (= msg (decode (encode msg)))))
  true)



;; @jonasseglare -- SEM added hints for performance -- now fast.  But my encode2 is faster.
(defn process [f ^String s]
  (let [b (StringBuilder. s)]
    (transduce (comp (map int)
                     (keep-indexed #(if (Character/isAlphabetic %2) [%1 (- (int
                                                                            (Character/toUpperCase
                                                                             ^int %2)) 65)])))
               (completing (fn [key [at x]]
                             (let [[diff y] (f key x)]
                               (.setCharAt b at (char (+ 65 (mod y 26))))
                               (inc diff)))) 0 s)
    (str b)))

(def jo-encode (partial process #(vector %2 (+ %1 %2))))
(def jo-decode (partial process (comp #(vector % %) - -)))


;; @mchampine -- fails on "GO 4 IT"
;; source elided as unfixable
