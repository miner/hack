(ns miner.gray
  (:require [clojure.edn :as edn]))


;; binary-reflected Gray code (BRGC)
;; https://en.wikipedia.org/wiki/Gray_code

;; first version, works but ugly
(defn gray-code-vec-XXX [nbits]
  (if (zero? nbits)
    [0]
    (let [n1 (dec nbits)
          gc1 (gray-code-vec n1)
          x (bit-shift-left 1 n1)]
      (into gc1 (map #(+ x %) (rseq gc1))))))

;; "reflective" gray code, recursive
;; Take n-1 gray code as base, reverse it and set high bit, append to original
(defn gray-code-vec [nbits]
  (if (zero? nbits)
    [0]
    (let [n1 (dec nbits)
          gc1 (gray-code-vec n1)]
      (into gc1 (map #(bit-set % n1) (rseq gc1))))))


;; mx is total range count, or 2^nbits
(defn gcode-vec
  ([mx] (gcode-vec mx 0 [0]))
  ([mx bit gv]
   (let [cnt (count gv)]
     (if (>= cnt mx)
       gv
       (recur mx (inc bit) (into gv (map #(bit-set % bit) (rseq gv))))))))





;; NOTE: it's much faster to generate the gray codes with bin->gray than with the recursive
;; algorithm

;; single gray encoding
(defn bin->gray [^long n]
  (bit-xor n (bit-shift-right n 1)))


(defn gbv [mx]
  (mapv bin->gray (range mx)))


(defn gray->bin [^long gray]
  (loop [mask (bit-shift-right gray 1) num gray]
    (if (zero? mask)
      num
      (recur (bit-shift-right mask 1) (bit-xor num mask)))))

;; sadly format doesn't work here
;; maybe should use cl-format

(let [pad064 (apply str (repeat 64 "0"))]
  (defn bstr
    ([n] (bstr n 8))
    ([n digits]
     ;; (assert (<= n 64))
     (let [s (Long/toBinaryString n)
           len (.length s)]
       (if (>= len digits)
         s
         (str (subs pad064 len digits) s))))))


(defn parse-bstr [bstr]
  (Long/parseLong bstr 2))
;; much faster than  (edn/read-string (str "2r" bstr)))


(defn smoke []
  (= (gray-code-vec 4)
     (map bin->gray (range 16))
     [0 1 3 2 6 7 5 4 12 13 15 14 10 11 9 8]))





(defn revstr [^String s]
  (-> (StringBuilder. s) .reverse .toString))
