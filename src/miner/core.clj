(ns miner.core
  (:refer-clojure)
  (:require [clojure.math :as m]
            [clojure.string :as str]))

;;; Many semi-useful things moved into the halfbaked lib (now on clojars)

;;; Clojure 1.11 will add a bunch of Math functions including `abs`
;;; absolute value
(defn abs-
  ([i] (if (neg? i) (- i) i))
  ([a b] (abs- (- a b))))

(defn ulp=
  "Tests that x is approximately equal to y within a tolerance of m*ulp(x)"
  ([x y] (ulp= x y 1.0))
  ([x y m]
   (or (= x y)
       (<= (abs- x y) (* m (clojure.math/ulp x))))))

(defn hexstr [n]
  (let [hs (clojure.string/upper-case (Long/toHexString n))
        len (count hs)
        phs (if (< len 16) (str (subs "0000000000000000" len) hs) hs)]
    (clojure.string/join " " (map #(subs phs % (+ % 4)) [0 4 8 12]))))

(defn dhexstr [^double d]
  (hexstr (Double/doubleToRawLongBits d)))

;;; Make a macro that automates this sort of counter (for testing side-effects and laziness)
#_
(let [counter (atom 0)]
  (defn fcount ([] @counter) ([n] (reset! counter n)))
  (defn f [x]
    (swap! counter inc)
    [(inc x)]))

(defn match-step [f coll]
  (every? identity (map = (rest coll) (map f coll))))

(defn match-indexed [f coll]
  (every? identity (map-indexed (fn [n val] (= (f n) val)) coll)))

(defn match-reduction [f coll]
  (every? identity (map = coll (reductions f (range (count coll))))))

;; unlike regular =, compares records as maps (ignoring record-type)
(defn isomorphic [x y]
  (or (= x y)
      (and x y
           (= (if (instance? clojure.lang.IRecord x) (into {} x) x)
              (if (instance? clojure.lang.IRecord y) (into {} y) y)))))

(defn reduce-nth [rf init coll]
  (let [initial (transient (vec init))
        cnt (count initial)
        ilast (dec cnt)]
    (if-not (pos-int? cnt)
      init
      (loop [result initial coll (seq coll) i 0]
        (if (seq coll)
          (recur (assoc! result i (rf (nth result i) (first coll)))
                 (rest coll)
                 (if (= i ilast) 0 (inc i)))
          (persistent! result))))))


;;; hacks for transients

;; Transient don't directly support peek! and update! so I wrote these convenience functions
;; to make it easier to port code from standard sequence functions to transients.  These
;; only work for transient vectors.  The bangs on peek! and reduce! are non-standard in that
;; it is not actually destructive.  However, the bang pattern for transient transformation
;; holds so that's what I call it.

;; Warning: untested code.  Maybe typos.

(defn peek! [tv]
  (nth tv (dec (count tv))))

;; See CLJ-1872 for empty? breakage on transient
(defn empty?! [tv] (zero? (count tv)))

(defn map! [f tv]
  (reduce #(assoc! % %2 (f (nth % %2))) tv (range (count tv))))

;; See CLJ-1848.  Full patch should handle arities as real update does, but call assoc!
(defn update!
  ([tv i f] (assoc! tv i (f (nth tv i))))
  ([tv i f x] (assoc! tv i (f (nth tv i) x)))
  ([tv i f x y] (assoc! tv i (f (nth tv i) x y)))
  ([tv i f x y z] (assoc! tv i (f (nth tv i) x y z)))
  ([tv i f x y z & more] (apply assoc! tv i (f (nth tv i) x y z more))))

(defn reduce! [rf init tv]
  (reduce #(rf % (nth tv %2)) init (range (count tv))))

;; Warning: maybe a bridge too far.  Think of it as interleaved reduce.  The `tinit` should
;; be a transient vector, whose count indicates how many lanes of reductions.  For example,
;; if there's three elements in tinit, every third item in collection goes into each lane.
;; Note: result is still transient!

(defn reduce-nth! [rf tinit coll]
  (let [cnt (count tinit)
        ilast (dec cnt)]
    (if-not (pos-int? cnt)
      tinit
      (loop [tresult tinit coll (seq coll) i 0]
        (if (seq coll)
          (recur (assoc! tresult i (rf (nth tresult i) (first coll)))
                 (rest coll)
                 (if (= i ilast) 0 (inc i)))
          tresult)))))


;; SEM need a whole file of transient work-arounds!



;;; Fix for partitionv in 1.12.0-alpha1.  See CLJ-2715.  My patch just added the missing
;;; `list`.  This version calculates the amount of padding needed, and avoids the call to
;;; `concat`.  Perhaps faster with less garbage, but I didn't do any performance testing.


(defn partv
  ([n coll]
   (partv n n coll))
  ([n step coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [p (into [] (take n) s)]
         (when (= n (count p))
           (cons p (partv n step (nthrest s step))))))))
  ([n step pad coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [p (into [] (take n) s)
             need (- n (count p))]
         (if (zero? need)
           (cons p (partv n step pad (nthrest s step)))
           (list (into p (take need) pad))))))))
