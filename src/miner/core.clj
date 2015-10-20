(ns miner.core
  (:refer-clojure))

;;; Many semi-useful things moved into the halfbaked lib (now on clojars)

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



