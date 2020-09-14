(ns miner.eseason)

;; https://gist.github.com/ericnormand/f43768bac0024936e0274cfd2ce06aae

;; Your job is to take a month (keyword) day (number) and a hemisphere (:north or :south) and
;; determine which season it is (return a keyword), according to this handy table.
;; 
;; Start       End         North  South
;; March 1     May 31      Spring Autumn
;; June 1      August 31   Summer Winter
;; September 1 November 30 Autumn Spring
;; December 1  February 29 Winter Summer



;; SEM:  note `day` is not needed, according to that table

(defn northern [month day]
  (case month
    (:march :april :may) :spring
    (:june :july :august) :summer
    (:september :october :november) :autumn
    (:december :january :february) :winter))

(defn southern [month day]
  (case month
    (:march :april :may) :autumn
    (:june :july :august) :winter
    (:september :october :november) :spring
    (:december :january :february) :summer))


(defn which-season1 [hemi month day]
  (let [fhemi (if (= hemi :south) southern northern)]
    (fhemi month day)))


;; slightly faster, pretty literal
;; SEM added arities for convenience, not part of problem description
(defn which-season
  ([month] (which-season :north month 1))
  ([month day] (which-season :north month day))
  ([hemi month day]
   (if (= hemi :south)
     (case month
       (:march :april :may) :autumn
       (:june :july :august) :winter
       (:september :october :november) :spring
       (:december :january :february) :summer)
     (case month
       (:march :april :may) :spring
       (:june :july :august) :summer
       (:september :october :november) :autumn
       (:december :january :february) :winter))))









(defn smoke-season [which-season]
  (assert (= (which-season :north :march 5) :spring))
  (assert (= (which-season :south :december 25) :summer))
  (assert (= (which-season :south :march 5) :autumn))
  (assert (= (which-season :north :december 25) :winter))
  true)






;;;; JUNK

(def months [:january :february :march :april :may :june :july :august :september :october
             :november :december])

(def month-indices (into {} (map-indexed #(vector %2 (inc %1)) months)))

(defn date-index [month day]
  (+ (* 100 (get month-indices month)) day))


(def raw-season-data [[:march 1 :may 31 :spring :autumn]
                      [:june 1 :august 31 :summer :winter]
                      [:september 1 :november 30 :autumn :spring]
                      [:december 1 :february 29 :winter :summer]])


(defn between? [a b x]
  (<= a x b))
