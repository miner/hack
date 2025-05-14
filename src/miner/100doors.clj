(ns miner.100doors
  (:require [clojure.data.int-map :as im]))

;;; https://rosettacode.org/wiki/100_doors#Clojure

;;; It's "cheating" to notice that only perfect squares remaing open.  You're supposed to
;;; simulate flipping every 1, 2, 3, etc. doors.  Cheating is refered to as "optimized".



;;; Note: problem statement used 1-based indexing.  Clojure is more natural with 0-based so I will
;;; fix at the end.

(defn sem-open-doors1 []
  (let [res (reduce (fn [doors ith]
                      (reduce (fn [ds n] (update ds n not)) doors (range ith 100 (inc ith))))
                    (vec (repeat 100 false))
                    (range 100))]
    (filterv #(get res (dec %)) (range 1 101))))

;;; vector-of :boolean not faster

(defn sem-od1 []
  (let [res (reduce (fn [doors ith]
                      (reduce (fn [ds n] (update ds n not)) doors (range ith 100 (inc ith))))
                    (vec (repeat 100 false))
                    (range 100))]
    (reduce (fn [dv i] (if (res i) (conj dv (inc i)) dv))
            []
            (range 100))))


;;; try with state as set of open doors (one-based), but not faster

(defn sem-open-doors2 []
   (reduce (fn [doors ith]
             (reduce (fn [ds i] (if (ds i) (disj ds i) (conj ds i)))
                     doors
                     (range ith 101 ith)))
           #{}
           (range 1 101)))

;;; a little bit faster with transients
(defn sem-open-doors3 []
  (persistent!
   (reduce (fn [doors ith]
             (reduce (fn [ds i] (if (ds i) (disj! ds i) (conj! ds i)))
                     doors
                     (range ith 101 ith)))
           (transient #{})
           (range 1 101))))



;;; not so great, really needs the transient to be fast
(defn sem-odds2 []
   (reduce (fn [doors ith]
             (reduce (fn [ds i]
                       (if (ds i) (disj ds i) (conj ds i)))
                     doors
                     (range ith 101 ith)))
           (im/dense-int-set)
           (range 1 101)))


;;; FASTEST by far
;;; with transient dense-int-set, state is one-based
;;; note functional dense-int-set direct test is surprisingly faster than contains?

(defn sem-odds []
  (persistent!
   (reduce (fn [doors ith]
             (reduce (fn [ds i] (if (ds i) (disj! ds i) (conj! ds i)))
                     doors
                     (range ith 101 ith)))
           (transient (im/dense-int-set))
           (range 1 101))))

;;; transduce not faster 


;;; state is vector of integer toggles -- even closed, odd open
;;; making it 1-based indexing, ignoring index 0
;;; OK but not fast

(defn sem-odi []
  (let [res (reduce (fn [doors ith]
                      (reduce (fn [ds n] (update ds n inc)) doors (range ith 101 ith)))
                    (vec (repeat 101 0))
                    (range 1 101))]
    (filterv #(odd? (res %)) (range 1 101))))
