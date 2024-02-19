(ns miner.balanced-ternary
  (:require [clojure.math :as m]))

;;; For amusement only.  This is not a practical system without hardware support like the
;;; Russians built many years ago.

;;; https://en.wikipedia.org/wiki/Balanced_ternary
;;;
;;; Trits (ternary digits) are T, 0, 1 with decimal values -1, 0, 1.  "T" resembles a
;;; ligature of "-" and "1".  Places are powers of 3, most significant on left.  Typically,
;;; leading zeroes are not used.

;;; bt is a balanced ternary string of trits (characters) T01.

#_
(require '[clojure.math.combinatorics :as mc])

(def trit {\T -1 \0 0 \1 1 -1 \T 0 0 1 1})

;;; fastest
(defn bt->long [bt]
  (reduce (fn [r t] (+ (* r 3) (trit t))) 0 bt))

(def btl bt->long)

(defn xbtd [bt]
  (transduce (map trit)
             (fn ([r t] (+ (* r 3) t)) ([r] r))
             0
             bt))

(defn slow-btd [bt]
  (reduce + 0 (map * (rseq (mapv trit bt)) (iterate #(* 3 %) 1))))

;;; slow
(defn pow-btd [bt]
  (let [mx (dec (count bt))]
    (reduce + 0 (map-indexed (fn [i t] (* (long (m/pow 3 (- mx i))) (trit t))) bt))))


;;; The wiki balanced ternary page has an algorigthm for converting standard base3 to
;;; balanced ternary: "Add 1 trit-by-trit from the first non-zero trit with carry, and then
;;; subtract 1 trit-by-trit from the same trit without borrow."  My code does some reversals
;;; for more convenient carrying.

(let [tneg {\T \1 \1 \T \0 \0}
      tdec {2 \1 1 \0 0 \T}]
  (defn long->bt [d]
    (if (zero? d)
      "0"
      ;; rtv = reversed ternary digits (0,1,2) in vec
      (let [rtv (loop [r (abs d) rtv []]
                  (if (zero? r)
                    rtv
                    (recur (quot r 3) (conj rtv (rem r 3)))))
            tcnt (count rtv)
            ;; rivc = reverse inc trits with carry (at peek)
            rivc (reduce (fn [r t]
                          (let [c (peek r)
                                p (inc (+ t c))]
                            (case (long p)
                              4 (conj (pop r) 1 1)
                              3 (conj (pop r) 0 1)
                              (conj (pop r) p 0))))
                        [0]
                        rtv)
            btv (if (zero? (peek rivc))
                  (mapv tdec (rseq (pop rivc)))
                  ;; the last carry must be a one, but don't dec it
                  (into [\1] (map tdec) (rseq (pop rivc))))]
        ;; if original d is neg, multiply trits by T (-1)
        (if (neg? d)
          (apply str (map tneg btv))
          (apply str btv))))))

(def lbt long->bt)


;;; T*T = 1
;;; 1*T = T
;;; 0*T = 0

(let [tneg {\T \1 \1 \T \0 \0}]
  (defn tbneg [bt]
    (apply str (map tneg bt))))


;;; intended for single digit (\T \0 \1) only, sort of a half adder with carry
(defn bt1+
  ([] [\0 \0])
  ([a] [\0 a])
  ([a b] (case a
           \T (case b \0 [\0 \T],  \1 [\0 \0],  \T [\T \1])
           \0 [\0 b]
           \1 (case b \T [\0 \0],  \0 [\0 \1],  \1 [\1 \T])))
  ([a b c] (cond (= c \0) (bt1+ a b)
                 (= a \0) (bt1+ c b)
                 (= b \0) (bt1+ a c)
                 :else (let [[ab1 ab0] (bt1+ a b)
                             [c1 c0] (bt1+ c ab0)]
                         (if (= ab1 \0)
                           [c1 c0]
                           ;; no carry possible
                           [(peek (bt1+ c1 ab1)) c0])))))


(defn bt-add
  ([] "0")
  ([a] a)
  ([aaa bbb]
   (let [acnt (count aaa)
         bcnt (count bbb)
         mx (max bcnt acnt)
         arv (into (vec (reverse aaa)) (repeat (- mx acnt) \0))
         brv (into (vec (reverse bbb)) (repeat (- mx bcnt) \0))
         rsv (reduce (fn [rc i]
                       (let [[d1 d0] (bt1+ (arv i) (brv i) (peek rc))]
                         (conj (pop rc) d0 d1)))
                     [\0]
                     (range mx))]
     ;;(println arv brv rsv)
     (if (= \0 (peek rsv))
       (apply str (rseq (pop rsv)))
       (apply str (rseq rsv))))))

;;; wiki: Second way to convert unbalanced ternary (what I call base3):
;;; "If a 2 is present in ternary, turn it into 1T"

;;; works but much slower
(let [tneg {\T \1 \1 \T \0 \0}]
  (defn lbt2 [d]
    (if (zero? d)
      "0"
      ;; rtv = reversed ternary digits (0,1,2) in vec
      (let [rtv (loop [r (abs d) rtv []]
                  (if (zero? r)
                    rtv
                    (recur (quot r 3) (conj rtv (rem r 3)))))
            tcnt (count rtv)
            bts-wv (reduce-kv (fn [r i t]
                                (let [working (peek r)]
                                  (if (= t 2)
                                    (conj (pop r) 
                                          (apply str "1T" (repeat i \0))
                                          (conj working \0))
                                    (conj (pop r) (conj working t)))))
                              [[]]
                              rtv)
            wv (peek bts-wv)
            btsum (if (empty? wv)
                    (reduce bt-add (pop bts-wv))
                    (reduce bt-add (apply str (rseq wv)) (pop bts-wv)))]
        ;; if original d is neg, multiply trits by T (-1)
        (if (neg? d)
          (apply str (map tneg btsum))
          btsum)))))




(def bt9 ["T01" "T10" "T1" "1T" "1T0" "10T"
          "T01T100T1" "T01T1001T" "T01T101T0" "T01T1010T" "T010T101T" "T010T11T0"
          "T010T110T" "T0101T1T0" "T0101T10T" "T011T010T" "T100T101T" "T100T11T0"
          "T100T110T" "T1001T1T0" "T1001T10T" "T101T010T" "T101T1T0" "T101T10T"
          "T11T010T" "1T1T010T" "100000000" "111111111" "1TTTTTTTT" "101101111"])


(def bd13 ["0" "1" "1T" "10" "11" "1TT" "1T0" "1T1" "10T" "100" "101" "11T" "110" "111"])

(def nbd13 ["0" "T" "T1" "T0" "TT" "T11" "T10" "T1T" "T01" "T00" "T0T" "TT1" "TT0" "TTT"])


;;; equivalent of all 1s in ternary
(defn bias [bt]
  (quot (reduce * (repeat (count bt) 3)) 2))
;; could use m/pow

;; not sure I understand how to use bias???


;;; Maybe better to use reverse vector of -1,0,1 so you can use built-in arithmetic.

;;; Definitely faster to stay in reverse-vector notation (with -1).  Only convert for
;;; display.  Or calculations.  Maybe even better to cache a decimal long as the peek value?

(defn btrv [bt]
  (let [btm {\T -1 \0 0 \1 1}]
    (mapv btm (reverse bt))))

(defn rvl [rv]
  (reduce (fn [r x] (+ (* 3 r) x)) 0 (rseq rv)))


(defn lrvbt [d]
  (if (zero? d)
    "0"
    ;; rtv = reversed ternary digits (0,1,2) in vec
    (let [tdec nil
          tmap (if (neg? d)
                 {1 \T, 0 \0, -1 \1}
                 {1 \1, 0 \0, -1 \T})
          rtv (loop [r (abs d) rtv []]
                  (if (zero? r)
                    rtv
                    (recur (quot r 3) (conj rtv (rem r 3)))))
            tcnt (count rtv)
            ;; rivc = reverse inc trits with carry (at peek)
            rivc (reduce (fn [r t]
                          (let [c (peek r)
                                p (inc (+ t c))]
                            (case (long p)
                              4 (conj (pop r) 1 1)
                              3 (conj (pop r) 0 1)
                              (conj (pop r) p 0))))
                        [0]
                        rtv)
            btv (if (zero? (peek rivc))
                  (mapv dec (rseq (pop rivc)))
                  ;; the last carry must be a one, but don't dec it
                  (into [1] (map dec) (rseq (pop rivc))))]
        ;; if original d is neg, multiply trits by T (-1)
          (apply str (map tmap btv)))))

  
(defn lrv [d]
  (if (zero? d)
    [0]
    ;; rtv = reversed ternary digits (0,1,2) in vec
    (let [tdec nil
          tmap (if (neg? d)
                 {1 \T, 0 \0, -1 \1}
                 {1 \1, 0 \0, -1 \T})
          rtv (loop [r (abs d) rtv []]
                  (if (zero? r)
                    rtv
                    (recur (quot r 3) (conj rtv (rem r 3)))))
            tcnt (count rtv)
            ;; rivc = reverse inc trits with carry (at peek)
            rivc (reduce (fn [r t]
                          (let [c (peek r)
                                p (inc (+ t c))]
                            (case (long p)
                              4 (conj (pop r) 1 1)
                              3 (conj (pop r) 0 1)
                              (conj (pop r) p 0))))
                        [0]
                        rtv)
          rv (if (zero? (peek rivc))
               (mapv dec (pop rivc))
               ;; the last carry must be a one, but don't dec it
               (conj (mapv dec (pop rivc)) (peek rivc)))]
      ;; if original d is neg, multiply trits by T (-1)
      (if (neg? d) (mapv - rv) rv))))
