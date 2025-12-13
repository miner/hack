(ns advent25.day2
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.math :as m])
  (:require [clojure.edn :as edn]))


(defn load-input []
  (let [filename "input25-2.txt"]
    (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
      (doall (take-while some? (repeatedly #(edn/read {:eof nil} in)))))))


(def example
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn parse-input [s]
  (partitionv 2 (read-string (str "[" (str/replace s "-" " ") "]"))))


(defn invalid-repeated? [n]
  (let [s (str n)
        cnt (count s)
        hcnt (quot cnt 2)]
    (and (even? cnt) (= (subs s 0 hcnt) (subs s hcnt)) true)))


(defn gen-invalid [ith]
  (when (pos? ith)
    (parse-long (str ith ith))))

;;; works
(defn next-invalid [n]
  (let [s (str n)
        cnt (count s)]
    (if (even? cnt)
      (let [half (parse-long (subs s 0 (quot cnt 2)))
            rep (gen-invalid half)]
        (if (> rep n)
          rep
          (gen-invalid (inc half))))
      (recur (long (m/pow 10 cnt))))))


(defn invalids-between [a b]
  (take-while #(<= % b) (rest (iterate next-invalid (max (dec a) 10)))))


(defn invalid-index [inv]
  (let [s (str inv)
        cnt (count s)]
    (parse-long (subs s 0 (quot cnt 2)))))

(defn inv-btw [a b]
  (let [i (if (<= a 11) 1 (invalid-index (next-invalid (dec a))))]
    (take-while #(<= % b) (map gen-invalid (iterate inc i)))))

(defn half-str [n]
  (let [s (str n)
        cnt (count s)]
    (when (even? cnt)
      (subs s 0 (quot cnt 2)))))

;;; ignore 0
(defn diglen [n]
  (long (m/ceil (m/log10 (inc n)))))

(defn day2 [s]
  (reduce + (sequence (mapcat #(apply inv-btw %) (parse-input s)))))

(def input2
  "990244-1009337,5518069-5608946,34273134-34397466,3636295061-3636388848,8613701-8663602,573252-688417,472288-533253,960590-988421,7373678538-7373794411,178-266,63577667-63679502,70-132,487-1146,666631751-666711926,5896-10827,30288-52204,21847924-21889141,69684057-69706531,97142181-97271487,538561-555085,286637-467444,93452333-93519874,69247-119122,8955190262-8955353747,883317-948391,8282803943-8282844514,214125-236989,2518-4693,586540593-586645823,137643-211684,33-47,16210-28409,748488-837584,1381-2281,1-19")

;;; correct 23560874270

;;; part 2:  Now, an ID is invalid if it is made only of some sequence of digits repeated at
;;; least twice. So, 12341234 (1234 two times), 123123123 (123 three times), 1212121212 (12
;;; five times), and 1111111 (1 seven times) are all invalid IDs.


;;; two times same as before
;;; 3 times -- count must be odd 3x
;;; any even mult is covered by 2x test
;;; any all the same is now invalid

;;; thinking in terms of generation
;;; 1 digit repeated -- all same, quick test and gen

;;; separate pattern width from number of reps

;;; all even rep counts are covered by part1 test

;;; odd reps  3,5,7,etc
;;; quick test if count supports cnt 9  works with 3wide x 3
;;; quick test front and back? then every

(defn gen-inv3 [ith]
  (when (pos? ith)
    (parse-long (str ith ith ith))))

(defn gen-inv-ith [rep pat]
  (when (and (pos? rep) (pos? pat))
    (parse-long (apply str (repeat rep pat)))))


(defn inv2 [n]

  )

  
