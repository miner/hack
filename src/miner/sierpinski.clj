;; Original https://rosettacode.org/wiki/Sierpinski_triangle#Clojure

(ns miner.sierpinski
  #_ (:require [clojure.contrib.math :as math]))


;; SEM -- I don't see the need to have different integer sizes.  Just use Long.

; Length of integer in binary
; (copied from a private multimethod in clojure.contrib.math)
(defmulti #^{:private true} integer-length class)

(defmethod integer-length java.lang.Integer [n]
  (count (Integer/toBinaryString n)))
(defmethod integer-length java.lang.Long [n]
  (count (Long/toBinaryString n)))
(defmethod integer-length java.math.BigInteger [n]
  (count (.toString n 2)))

;; SEM to replace math/expt
(defn two-to-n [n]
  (bit-set 0 n))


(defn sierpinski-triangle [order]
  (loop [size (two-to-n order)
         v    (two-to-n (- size 1))]
    (when (pos? size)
      (println
       (apply str (map #(if (bit-test v %) "*" " ")
		       (range (integer-length v)))))
      (recur 
       (dec size)
       (bit-xor (bit-shift-left v 1) (bit-shift-right v 1))))))

(comment

  (sierpinski-triangle 4)

  )


;; SEM -- my self-contained version, limited to Long (order 5)

(defn sierp [order]
  {:pre [(<= 0 order 5)]}
  (loop [size (bit-set 0 order)
         v    (bit-set 0 (dec size))]
    (when (pos? size)
      (dotimes [i Long/SIZE]
        (print (if (bit-test v i) "*" " ")))
      (println)
      (recur 
       (dec size)
       (bit-xor (bit-shift-left v 1) (bit-shift-right v 1))))))


;; But I don't like mixing generation with i/o.  It's faster, but not so easy to test and
;; modify.


;; pure generation, no i/o
;; returns vector of longs, with appropriate bits set.  Basically same idea, but you have to
;; print with another function.
(defn sierp2 [order]
  {:pre [(<= 0 order 5)]}
  (loop [size (dec (bit-set 0 order))
         vs   [(bit-set 0 size)]]
    (if (zero? size)
      vs
      (let [v (peek vs)]
        (recur (dec size)
               (conj vs (bit-xor (bit-shift-left v 1)
                                 (bit-shift-right v 1))))))))

;; slower
(defn sierp3 [order]
  (let [size (dec (bit-set 0 order))]
    (first (drop size (iterate (fn [vs]
                                 (let [v (peek vs)]
                                   (conj vs (bit-xor (bit-shift-left v 1)
                                                     (bit-shift-right v 1)))))
                               [(bit-set 0 size)])))))



;; slightly faster than sierp2,
;; each "line" is a long int with raster bits set for stars
(defn sierp-lines [order]
  {:pre [(<= 0 order 5)]}
  (let [size (dec (bit-set 0 order))]
    (reduce (fn [lines _]
              (let [prev (peek lines)]
                (conj lines (bit-xor (bit-shift-left prev 1)
                                     (bit-shift-right prev 1)))))
            [(bit-set 0 size)]
            (range size))))

;; I like the X/- look better than */space.
;; All the i/o into one function.
(defn print-tri [order]
  (doseq [line (sierp-lines order)]
    (dotimes [i Long/SIZE]
      (print (if (bit-test line i) "X" "-")))
    (println)))


