(ns eric.chocolates)

;; https://gist.github.com/ericnormand/1d55a86288b53dbf06c83f2190aee154

;; Your task is to write a function that takes three arguments:
;; 
;; smalls - The number of small chocolates available in the inventory.
;; larges - The number of large chocolates available in the inventory.
;; mass - The total mass of the ordered box.
;; 
;; Small (2 grams each)
;; Large (5 grams each)
;; 
;; Your function should return a map containing:
;; 
;; {:small ;; the number of small chocolates
;;  :large ;; the number of large chocolates
;; }
;; Or nil if the total mass is not possible.
;; 
;; One other constraint is you should strive to have the fewest number of chocolates that total
;; to the target mass. That means you should prefer large chocolates over small chocolates if
;; you have the choice.


(defn assemble [smalls larges mass]
  (when (<= mass (+ (* larges 5) (* smalls 2)))
    (loop [lg (min (quot mass 5) larges)]
      (let [sm (min (quot (- mass (* lg 5)) 2) smalls)]
        (if (= mass (+ (* lg 5) (* sm 2)))
          {:large lg :small sm}
          (when (pos? lg)
            (recur (dec lg))))))))

;;(println "LG" lg "SM" sm "Tot" (+ (* lg 5) (* sm 2)))


(defn assemble1 [smalls larges mass]
  (let [total (fn [lgs sms] (+ (* lgs 5) (* sms 2)))]
    (when (<= mass (total larges smalls))
      (loop [lg (min (quot mass 5) larges)]
        (let [sm (min (quot (- mass (* lg 5)) 2) smalls)]
          (if (= mass (total lg sm))
            {:large lg :small sm}
            (when (pos? lg)
              (recur (dec lg)))))))))


;; slower, maybe nicer
(defn assemble2 [smalls larges mass]
  (let [total (fn [lgs sms] (+ (* lgs 5) (* sms 2)))
        sol? (fn [lgs sms] (= mass (total lgs sms)))]
    (when (<= mass (total larges smalls))
      (first (for [lg (range (min (quot mass 5) larges) -1 -1)
                   :let [sm (min (quot (- mass (* lg 5)) 2) smalls)]
                   :when (sol? lg sm)]
               {:large lg :small sm})))))
   



;; The one true assert=.  I have lots of variations but this is the best.
(defmacro assert=
  ([] true)
  ([form result & more]
   `(do (assert (= ~form ~result))
        (assert= ~@more))))

(defn smoke-ass [assemble]
  (assert=
   (assemble 100 100 2)  {:small 1 :large 0}
   (assemble 100 100 1)  nil
   (assemble 100 100 10) {:small 0 :large 2}
   (assemble 10 2 20)    {:large 2 :small 5}
   (assemble 3 3 20) nil
   (assemble 4 4 21)  {:large 3 :small 3}
   (assemble 3 10 22) {:large 4 :small 1}
   (assemble 10 3 22) {:large 2 :small 6}
   (assemble 10 4 29) {:large 3 :small 7}
   (assemble 10 1 14) {:large 0, :small 7}
   ))


;;; Better idea than mine.  Calcs left-over q and deals with it directly.  No need to loop.
;;; @jonasseglare with slight SEM hack
(defn jo-assemble [max-smalls max-larges mass]
  (let [prelarge (min max-larges (quot mass 5))
        leftover (- mass (* 5 prelarge))
        q (mod leftover 2)
        small (+ (* 3 q) (quot leftover 2))]
    (when (and (<= q prelarge) (<= small max-smalls))
      {:large (- prelarge q) :small small})))

