(ns advent25.day1
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn]))


(defn load-input []
  (let [filename "input25-1.txt"]
    (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
      (doall (take-while some? (repeatedly #(edn/read {:eof nil} in)))))))

(defn convert [lr-sym]
  (let [s (str lr-sym)
        n (parse-long (subs s 1))]
    (case (nth s 0)
      \L (- n)
      \R n
      nil)))

(defn rot [position rotate]
  (mod (+ position rotate) 100))

(defn day1 []
  (count (filter zero? (reductions rot 50 (map convert (load-input))))))



(def xinput (map convert (load-input)))

;;; SEM: tricky if starting at 0 and going neg

(defn day1-2 []
  (peek (reduce (fn [[p z] rot]
                  (let [px (+ p rot)
                        z2 (cond (zero? px) (inc z)
                                 (neg? px) (+ (if (zero? p) z (inc z)) (quot px -100))
                                 (> px 99) (+ z (quot px 100))
                                 :else z)]
                    [(mod px 100) z2]))
                [50 0]
                (map convert (load-input)))))

;;; correct answer for part2 is 6907

;;; Lots of mistakes:
;;; 8217 is too high
;;; 7747 is too high
;;; 7509 is wrong
;;; 7039 is wrong
;;; 7035 is wrong
