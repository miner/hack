(ns miner.algebraic
  (:require [clojure.string :as str]))


;;; 09/23/25  17:12 by miner -- 

;;; https://en.wikipedia.org/wiki/Algebraic_notation_(chess)
;;;
;;; Each square of the board is identified by a unique coordinate pair —- a letter and a
;;; number -— from White's point of view. The vertical columns of squares, called files, are
;;; labeled 'a' through 'h' from White's left (the queenside) to right (the kingside). The
;;; horizontal rows of squares, called ranks, are numbered 1 to 8 starting from White's side
;;; of the board. Thus each square has a unique identification of file letter followed by
;;; rank number. For example, the initial square of White's king is designated as "e1".


;;; converts \a-\h \A-\H to 1-8, and \0-\9 to 0-9
(defn ah18 [ch]
  (bit-and 2r1111 (long ch)))

;;; returns 0-7 which fits in 3 bits
(defn ah0 [ch]
  (dec (ah18 ch)))


(defn ah18? [ch]
  (let [res (ah18 ch)]
    (and (<= 1 res 8)
         (let [x (long ch)]
           (or (<= (long \a) x (long \z))
               (<= (long \A) x (long \Z))
               (<= (long \1) x (long \8)))))))



;;; x and y are zero-based, but algebraic notation is 1-based
(defn alg0 [file0 rank0]
  (str (nth "abcdefgh" file0) (inc rank0)))

(defn xy0 [alg]
  (assert (and (string? alg) (= (count alg) 2)))
  (vector (case (nth alg 0)
            (\a \A) 0
            (\b \B) 1
            (\c \C) 2
            (\d \D) 3
            (\e \E) 4
            (\f \F) 5
            (\g \G) 6
            (\h \H) 7)
          (dec (parse-long (subs alg 1)))))


(defn alg? [alg]
  (and (string? alg) (= (count alg) 2) (every? ah18? alg)))




;;; lfr is long encoded rank and file in low byte according to algebraic chars, but zero based.
;;; "c5" ==> is 0/4 file/rank with high 3 bits = 2 for \c (file) + low 3 bits = 4 (rank).
;;; capitals are translated to canonical lower in storage so E5 => e5

(defn lfr [algstr]
  (bit-or (ah0 (nth algstr 0))
          (bit-shift-left (ah0 (nth algstr 1)) 3)))

(defn rank0 [lfr]
  (bit-shift-right lfr 3))

(defn file0 [lfr]
  (bit-and 2r111 lfr))

(defn alg [lfr]
  (str (char (bit-or 2r1100000 (inc (bit-and 2r111 lfr))))
       (char (bit-or 2r110000 (inc (bit-shift-right lfr 3))))))


;;; FIXME looser than necessary
(defn algstr
  ([lfr-str-vec]
   (cond (string? lfr-str-vec) (str/lower-case lfr-str-vec)
         (int? lfr-str-vec) (alg lfr-str-vec)
         (vector? lfr-str-vec) (apply alg0 lfr-str-vec)))
  ([file0 rank0] (alg0 file0 rank0)))


(def a1 (lfr "a1"))
(def h8 (lfr "h8"))
(def e5 (lfr "e5"))
(def c7 (lfr "c7"))


(defn lfr-up [lfr]
  (when (< (rank0 lfr) 7)
    (+ lfr 2r1000)))


(defn lfr-down [lfr]
  (when (pos? (rank0 lfr))
    (- lfr 2r1000)))

(defn lfr-left [lfr]
  (when (pos? (file0 lfr))
    (dec lfr)))

(defn lfr-right [lfr]
  (when (< (file0 lfr) 7)
    (inc lfr)))   
