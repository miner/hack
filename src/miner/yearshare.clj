(ns miner.yearshare)

;;; https://www.johndcook.com/blog/2022/05/07/day-of-the-week/

;;; Ways to calculate day of week for any year.  Originally in Python.  Ported to Clojure.
;;; Intended to be easy to remember so you can do it in your head.  Well, not so easy for me.

;; Take the last two digits of the year and add the number of times 4 divides that number.
;; Add a constant corresponding to the month.
;; Add the day of the month.
;; Subtract 1 for January or February of a leap year for .
;; Take the remainder by 7.

;; Month constants:
;; | January | 6 | February | 2 | March     | 2 |
;; | April   | 5 | May      | 0 | June      | 3 |
;; | July    | 5 | August   | 1 | September | 4 |
;; | October | 6 | November | 2 | December  | 4 |

;; Day numbers: 0 for Sunday, 6 for Saturday

;; January is month 1

;;; But only for this century! For dates in the 20th century, add 1. For dates in the 22nd
;;; century, subtract 1.

(def month-kw
  [:January :February :March :April :May :June :July :August :September :October :November
   :December])

(def kw->month (zipmap month-kw (range)))

(def day-kw [:Sunday :Monday :Tuesday :Wednesday :Thursday :Friday :Saturday])

(def kw->day (zipmap day-kw (range)))



(defn leap-year? [yr]
  (and (zero? (mod yr 4)) (not (zero? (mod yr 100)))))

(defn dow [day month year]
  (let [y (rem year 100)
        y4 (quot y 4)
        ;; month-constants are one-based
        mc (nth [nil 6 2 2 5 0 3 5 1 4 6 2 4] month)
        leap (if (and (leap-year? year) (<= month 2)) -1 0)]
    (rem (+ y y4 mc day leap) 7)))

(defn day-of-week [day month year]
  (day-kw (dow day month year)))


(import java.time.LocalDate)

;; much faster using Java 8 API for LocalDate.  And probably good for any reasonable date,
;; not just this century!
(defn jdow [day month year]
  (.getValue (.getDayOfWeek (java.time.LocalDate/of ^int year ^int month ^int day))))




;;; https://www.johndcook.com/blog/2022/05/23/year-share/
;;;  Y is the last two digits
;;; on the year in the 2000s.

(defn year-share [y]
  (+ y (quot y 4)))

;;; Not worth coding the rest.  I don't think I would memorize them in any case.

"
    def method0(y):
        return y + y//4
    
    def method1(y):
        if y % 2 == 1:
            y += 11
        y /= 2
        if y % 2 == 1:
            y += 11
        y %= 7
        return 7 - y
    
    def method2(y):
        parity1 = y % 2
        if parity1:
            y -= 3
        y /= 2
        parity2 = y % 2
        if parity1 != parity2:
            y -= 3
        return -y 
    
    def method3(y):
        t = y % 12
        return y // 12 + t + t//4
    
    def method4(y):
        r = y % 4
        y -= r
        return r - y//2
    
    def method5(y):
        # placeholder to maintain the numbering in the paper
        return method0(y)
    
    def method6(y):
        r = y % 4
        y -= r # i.e. latest leap year
        t = y // 10
        u = y % 10
        return 2*t - u//2 + r
    
    def method7(y):
        t = y // 10
        u = y % 10
        x = (2*t + u) // 4
        x += u
        # The paper says to return 2t - x but it should be the opposite.
        return x - 2*t
    
    def method8(y):
        t = y // 10
        u = y % 10
        p = t % 2
        return (2*t + 10*p + u + (2*p + u)//4) % 7
    
    def method9(y):
        t = y // 10
        u = y % 10    
        return u - t + floor(u/4 - t/2)
    
"
