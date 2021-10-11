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



;; SEM:  note `day` is not needed, according to that table.
;; I did another version lower down that uses more precise dates.


;; As we don't expect to change months or seasons, we can use a literal translation.
(defn which-season [hemi month day]
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
      (:december :january :february) :winter)))


;; SEM added arities for convenience, not part of problem description
(defn which-season2
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




;;; SEM: It's a better problem if the days matter.  The 21st of the month is a better
;;; average day for the soltices.  These are better dates:

;; Start        End          North  South
;; March 20     June 20      Spring Autumn
;; June 21      September 22 Summer Winter
;; September 23 December 20  Autumn Spring
;; December 21  March 19     Winter Summer


;;; MMDD for convenient literal longs
(defn date-index [month day]
  (+ day
     (case month
       :january 100
       :february 200
       :march 300
       :april 400
       :may 500
       :june 600
       :july 700
       :august 800
       :september 900
       :october 1000
       :november 1100
       :december 1200)))


(defn season2 [hemi month day]
  (let [ind (date-index month day)]
    (if (= hemi :south)
      (condp >= ind
        319 :summer
        620 :autumn
        922 :winter
        1220 :spring
        :summer)
      (condp >= ind
        319 :winter
        620 :spring
        922 :summer
        1220 :autumn
        :winter))))


(defn season3 [hemi month day]
  (let [ind (date-index month day)]
    (if (= hemi :south)
      (condp <= ind
        1221 :summer
        923 :spring
        621 :winter
        320 :autumn
        :summer)
      (condp <= ind
        1221 :winter
        923 :autumn
        621 :summer
        320 :spring
        :winter))))







;; Older junk

(def months [:january :february :march :april :may :june :july :august :september :october
             :november :december])

(def month-indices (into {} (map-indexed #(vector %2 (* 100 (inc %1))) months)))

(defn date-index1 [month day]
  (+ (get month-indices month) day))


(def raw-season-data [[:march 20 :june 20 :spring :autumn]
                      [:june 21 :september 22 :summer :winter]
                      [:september 23 :december 20 :autumn :spring]
                      [:december 21 :march 19 :winter :summer]])

(defn between? [a b x]
  (<= a x b))


(defn season [hemi month day]
  (let [ind (date-index month day)]
    (if (= hemi :south)
      (cond (between? 101 319 ind) :summer
            (between? 320 620 ind) :autumn
            (between? 621 922 ind) :winter
            (between? 923 1220 ind) :spring
            (between? 1221 1231 ind) :summer)
      (cond (between? 101 319 ind) :winter
            (between? 320 620 ind) :spring
            (between? 621 922 ind) :summer
            (between? 923 1220 ind) :autumn
            (between? 1221 1231 ind) :winter))))

