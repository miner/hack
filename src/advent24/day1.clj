(ns advent24.day1
  (:require [clojure.java.io :as io])
  (:require [clojure.edn :as edn]))


;;; https://adventofcode.com/2024/day/1
;;; using Google login

;;; file in hack/resources/
(def input-24-1-file-name "input24-1.txt")

(defn load-input [filename]
  (with-open [in (java.io.PushbackReader. (io/reader (io/resource filename)))]
    (doall (take-while some? (repeatedly #(edn/read {:eof nil} in))))))

(defn total-dist [xs ys]
  (reduce + 0 (map abs (map - (sort xs) (sort ys)))))


(defn day1-part1 []
  (let [raw-data (load-input input-24-1-file-name)
        column1 (take-nth 2 raw-data)
        column2 (take-nth 2 (rest raw-data))]
    (total-dist column1 column2)))

;; (day1-part1)
;; 1646452


(defn similarity [freqs n]
  (* n (get freqs n 0)))


(defn day1-part2 []
  (let [raw-data (load-input input-24-1-file-name)
        column1 (take-nth 2 raw-data)
        freqs (frequencies (take-nth 2 (rest raw-data)))]
    (reduce + 0 (map #(similarity freqs %) column1))))

;; (day1-part2)
;; 23609874
