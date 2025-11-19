(ns miner.sunlight
  (:require [clojure.math :as m]))

#_ (require '[clojure.math :as m])

;;; If you really want to implement something useful, see:
;;; https://github.com/klausbrunner/solarpositioning
;;; for Java lib with calcs


;;; for online numbers, use this:
;;; https://www.suncalc.org/#/27.3853,-82.3889,13/2024.01.01/13:22/4/3


;;; cafe window 96" high x 72" wide
;;; 22" below, 25" above, top at 119" from floor
;;; 156" overhang, 144" tall lanai

;;; side window (south) - same height and clearance.  basically half width
;;; 41" from back corner to left edge


;;; above window angle is
(def top-window-angle (m/atan (/ 25.0 156.0)))
;;; 0.15890526218119308

(def bottom-window-angle (m/atan (/ (+ 25.0 96.0) 156.0)))
;;; 0.6597104286357587
;;; roughly 38d

;;; more roughly 13ft overhang, 12ft high
;;; roughly 32d elevation angle
(def rough-elevation-degrees (m/to-degrees (m/atan (/ 12 13))))
;;; 42.7 d

;;; My notes copied from wildblue.txt
;; (adjusted for DST, ext Dec)
;; 
;; Date    Sunrise        Sunset    Elev (midday)
;; Mar 21  07:31  89°, 19:41 271°   63°  1:30pm
;; June 21 06:35  62°, 20:27 297°   86°  1:30pm
;; Sept 21 07:17  89°, 19:26 270°   63°  1:30pm
;; Dec 21	07:15 116°, 17:40 243°   39°  12:30pm
;; 
;; 
;; considering summer solstice for max sun
;; due south almost 90d elev at 1:30pm (solar noon)
;; sets azimuth 300d at 8:30, almost 14 hr of sunlight
;; sun infiltration will be about same as height of lanai at 5pm
;; 
;; more numbers for June 22 (all numbers are roughly to +/- 1d)
;; 2:00pm, 82d elev, 240d az
;; 2:30pm, 76d elev, 256d az
;; 4:00pm, 56d elev, 272d az
;; 5:00pm, 43d elev, 277d az
;; 6:00pm, 30d elev, 282d az
;; 7:00pm, 17d elev, 288d az
;; 8:00pm, 5d elev, 293d az
;; 8:28pm, sunset, 297d az
;; 
;; Mar 21
;; 3:00pm, 57d elev, 220d az
;; 4:00pm, 46d elev, 238d az
;; 5:00pm, 35d elev, 250d az
;; 6:00pm, 22d elev, 259d az
;; 7:42pm, sunset, 271d az
;; 
;; will need good blinds or shutters in bedroom
;; https://www.levolor.com/custom-cellular-shades.html#overview
;; 
;; calculating position of sun
;; https://gml.noaa.gov/grad/solcalc/
;; 
;; a better one for tables (use GMT-4 for EDT)
;; https://aa.usno.navy.mil/data/AltAz
;; 
;; Visual sun path, slider for time
;; https://www.suncalc.org/#/27.3784,-82.3876,17/2024.06.21/13:27/5/3

(defn sun-length-ft [elev-deg]
  (if (< elev-deg 33)
    100.0
    (- (* 12.0 (m/asin (m/to-radians (- 90.0 elev-deg)))) 13.0)))

  
;;; Break is about 40d, less than that and sun penetrates window.  Higher than 40d elevation
;;; is blocked by overhang
