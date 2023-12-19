(ns advent23.day15
  (:require [clojure.string :as str]))

;;; solution by Mike Zamansky
;;; https://gitlab.com/zamansky/advent2023/-/blob/main/src/day15.clj


(def sample-str "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn calc-hash [word]
  (loop [ [x & xs]  word hash 0]
    (if (nil? x)
      hash
      (recur xs (rem (* (+ (int x) hash) 17) 256)))))

(defn part1 [data]
  (apply + (map calc-hash (str/split data #","))))

;;; SEM version

(defn sem-calc [word]
  (reduce (fn [r b] (bit-and 0xFF (* (+ (long b) r) 17))) 0 (seq word)))

(defn sem1 [s]
  (reduce + 0 (mapv sem-calc (str/split s #","))))

(defn sem2 [s]
  (let [[r h]  (reduce (fn [[r h] c]
                         (if (= c \,)
                           [(+ r h) 0]
                           [r (bit-and 0xFF (* (+ (long c) h) 17))]))
                       [0 0]
                       (seq s))]
    (+ r h)))


(defn sem22 [s]
  (reduce + (reduce (fn [stack c]
                         (if (= c \,)
                           [(reduce + stack) 0]
                           (conj (pop stack) (bit-and 0xFF (* (+ (long c) (peek stack)) 17)))))
                       [0 0]
                       (seq s))))


(defn sem3 [s]
  (transduce (comp (partition-by #{\,}) (remove #(= (peek %) \,)) (map sem-calc))
             +
             0
             s))




(defn sem-calc2 [word]
  (if (= (first word) \,)
    0
    (reduce (fn [r b] (bit-and 0xFF (* (+ (long b) r) 17))) 0 (seq word))))

(defn sem4 [s]
  (transduce (comp (partition-by #(= % \,)) (map sem-calc2))
             +
             0
             s))
