(ns miner.efriday
  (:import (java.time LocalDate DayOfWeek)))


;; Clojure Challenge - Friday the 13th
;; https://gist.github.com/ericnormand/ac5b2ce76ec0dbf354b869579f983488
;;
;; A function that tells you the next upcoming Friday the 13th.
;; A function that tells you all the Friday the 13ths in a given year.
;; A predicate that tells you if a given year + month had a Friday the 13th.
;; A predicate to know if a given date (year, month, day) is a Friday the 13th.
;; A function that when the next upcoming Friday the 13th falls in a given month.
;; 
;; If you're working in JVM Clojure, use the Java 8 java.time package.

(defn local-date
  ([] (LocalDate/now))
  ([year month day] (LocalDate/of ^int year ^int month ^int day)))

(defn year-month-day [^LocalDate locdate]
  (vector (.getYear locdate) (.getMonthValue locdate) (.getDayOfMonth locdate)))

(defn friday? [^LocalDate locdate]
  (= (.getDayOfWeek locdate) DayOfWeek/FRIDAY))

(defn next-friday-the-13th
  ([] (next-friday-the-13th (local-date)))
  ([locdate]
  (let [[yr mon day] (year-month-day locdate)]
    (if (and (<= day 13) (friday? (local-date yr mon 13)))
      (local-date yr mon 13)
      (or (first (for [m (range (inc mon) 13)
                       :let [ld (local-date yr m 13)]
                       :when (friday? ld)]
                   ld))
          (first (for [y (iterate inc (inc yr))
                       m (range 1 13)
                       :let [ld (local-date y m 13)]
                       :when (friday? ld)]
                   ld)))))))
             

(defn fridays-the-13th [year]
  (for [m (range 1 13)
        :let [ld (local-date year m 13)]
        :when (friday? ld)]
    ld))

(defn has-friday-the-13th? [year month]
  (friday? (local-date year month 13)))

(defn friday-the-13th? [year month day]
  (friday? (local-date year month day)))

(defn next-friday-the-13th-in-month? [month]
  (let [next-fri-13 (next-friday-the-13th)]
    (= month (.getMonthValue ^LocalDate next-fri-13))))
  


;; surprisingly a bit faster that the .plusMonths style or the iterate style
(defn lazy-fridays-the-13th-from [locdate]
  (let [[yr mon day] (year-month-day locdate)
        mon (if (> day 13) (inc mon) mon)]
    ;; don't need to adjust yr as logic will cover mon=13
    (concat (for [m (range mon 13)
                  :let [ld (local-date yr m 13)]
                  :when (friday? ld)]
              ld)
            (for [y (iterate inc (inc yr))
                  m (range 1 13)
                  :let [ld (local-date y m 13)]
                  :when (friday? ld)]
              ld))))


(defn w13s-from [^LocalDate locdate]
  (sequence (filter #(= (.getDayOfWeek ^LocalDate %) DayOfWeek/FRIDAY))
            (cond-> (iterate #(.plusMonths ^LocalDate % 1) (.withDayOfMonth locdate 13))
               (> (.getDayOfMonth locdate) 13) rest)))



(defn smoke-friday []
  (assert (friday-the-13th? (local-date 2021 8 13)))
  (assert (not (friday-the-13th? (local-date 2021 5 13)))))


#_
(sequence (comp (mapcat fridays-the-13th) (map str)) (range 2010 2030))


#_
(defn day-of-week [^LocalDate locdate]
  (.getDayOfWeek locdate))

#_
(defn friday-13? [^LocalDate locdate]
  (and (= (.getDayOfMonth locdate) 13)
       (= (.getDayOfWeek locdate) DayOfWeek/FRIDAY)))

