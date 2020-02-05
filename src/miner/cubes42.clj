(ns miner.cubes42)

;; https://www.sciencealert.com/mathematicians-solve-a-long-standing-42-problem-using-planetary-supercomputer 

;; It took over a million hours of computing time, but the two mathematicians found their solution.
;;
;; X = -80538738812075974
;;
;; Y = 80435758145817515
;;
;; Z = 12602123297335631
;;
;; Full equation is (-80538738812075974)^3 + 80435758145817515^3 + 12602123297335631^3 = 42.


;; Of course, the mathematics of searching for a solution is much harder than simply
;; confirming the calculation.


(def x42 -80538738812075974)

(def y42 80435758145817515)

(def z42 12602123297335631)

;; I prefer to see a simple long if it's possible
(defn prefer-long [x]
  (cond (int? x) x
        (<= Long/MIN_VALUE x Long/MAX_VALUE) (long x)
        :else x))

;; need to allow bigints
(defn sum-cubes [& xs]
  (prefer-long (reduce +' (map #(*' % % %) xs))))


#_ (sum-cubes x42 y42 z42)
;;=>  42
