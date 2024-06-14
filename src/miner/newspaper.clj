(ns miner.newspaper
  (:require [clojure.math :as cm]))

;;; Simple exercise inspired by layout of newspaper pages.  Four pages to a sheet.  Two per
;;; side.  Stack the sheets and fold in middle.

;;; cnt is the total number of pages -- will round up to generate a mulitple of 4.
;;; One-based page numbering.

(defn newspaper-seq [page-cnt]
  (when (pos? page-cnt)
    (let [sheet-cnt (quot (+ page-cnt 3) 4)
          sum (inc (* 4 sheet-cnt))]
      (for [p (range 1 (inc (* 2 sheet-cnt)) 2)]            
        [(- sum p) p (inc p) (- sum (inc p))]))))
    


;;; example for 12 pages, print first two in order, flip over, print next two in order
;;;    12 1 2 11
;;;    10 3 4 9
;;;    8  5 6 7
;;;    [[12 1 2 11] [10 3 4 9] [8 5 6 7]]



(defn ceil4 [n]
  (quot (+ n 3) 4))

;;; faster to use double cm/ceil but maybe not as nice to read
(defn c4 [n]
  (long (cm/ceil (/ n 4.0))))


(defn quot-up [n d]
  (quot (+ n (dec d)) d))

(defn ceild [n d]
  (long (cm/ceil (/ (double n) (double d)))))
