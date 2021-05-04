(ns miner.core
  (:refer-clojure))

;;; Many semi-useful things moved into the halfbaked lib (now on clojars)

;;; absolute value
(defn abs
  ([i] (if (neg? i) (- i) i))
  ([a b] (abs (- a b))))


;;; Make a macro that automates this sort of counter (for testing side-effects and laziness)
(let [counter (atom 0)]
  (defn fcount ([] @counter) ([n] (reset! counter n)))
  (defn f [x]
    (swap! counter inc)
    [(inc x)]))


(defn foo [x] (* 3 x))

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

