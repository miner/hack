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


(defn ah18? [ch]
  (let [res (ah18 ch)]
    (and (<= 1 res 8)
         (let [x (long ch)]
           (or (<= (long \a) x (long \z))
               (<= (long \A) x (long \Z))
               (<= (long \1) x (long \8)))))))



;;; x and y are zero-based, but algebraic notation is 1-based
(defn alg0 [x0 y0]
  (str (nth "abcdefgh" x0) (inc y0)))

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

(defn xy1 [alg]
  (assert (and (string? alg) (= (count alg) 2)))
  (vector (ah18 (nth alg 0))
          (ah18 (nth alg 1))))

(defn alg1 [x1 y1]
  (assert (and (<= 1 x1 8) (<= 1 y1 8)))
  (str (char (bit-or 2r1100000 x1))
       y1))


;;; lrf is long encoded rank and file according to algebraic chars
;;; "e5" ==> high byte is \e (file) + low byte 5 (rank)
;;; capitals are translated to canonical lower in storage so E5 => e5

(defn lrf [algstr]
  (assert (and (string? algstr) (= (count algstr) 2)))
  (bit-or (ah18 (nth algstr 0))
          (bit-shift-left (ah18 (nth algstr 1)) 4)))


(defn rank0 [lrf]
  (dec (bit-and 2r1111 (bit-shift-right lrf 4))))

(defn file0 [lrf]
  (dec (bit-and 2r1111 lrf)))

(defn alg [lrf]
  (str (char (bit-or 2r1100000 (bit-and 2r11111 lrf)))
       (char (bit-or 2r110000 (bit-shift-right lrf 4)))))


;;; FIXME looser than necessary
(defn algstr
  ([lrf-str-vec]
   (cond (string? lrf-str-vec) (str/lower-case lrf-str-vec)
         (int? lrf-str-vec) (alg lrf-str-vec)
         (vector? lrf-str-vec) (apply alg0 lrf-str-vec)))
  ([x0 y0] (alg0 x0 y0)))


(def a1 (lrf "a1"))
(def h8 (lrf "h8"))
(def e5 (lrf "e5"))
(def c7 (lrf "c7"))


(defn lrf-up [lrf]
  (when (< (rank0 lrf) 7)
    (+ lrf 0x100)))

(defn lrf-down [lrf]
  (when (pos? (rank0 lrf))
    (- lrf 0x100)))

(defn lrf-left [lrf]
  (when (pos? (file0 lrf))
    (dec lrf)))

(defn lrf-right [lrf]
  (when (< (file0 lrf) 7)
    (inc lrf)))
