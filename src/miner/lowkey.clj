;; experiments with CLJ-99

(ns miner.lowkey
  (:require [clojure.core.reducers :as r]))

(defn orig-min-key
   "Returns the x for which (k x), a number, is least."
   ([k x] x)
   ([k x y] (if (< (k x) (k y)) x y))
   ([k x y & more]
      (reduce #(orig-min-key k %1 %2) (orig-min-key k x y) more)))

(defn patch-min-key
   "Returns the x for which (k x), a number, is least."
   ([k x] x)
   ([k x y] (if (< (k x) (k y)) x y))
   ([k x y & more]
   (let [kx (k x) ky (k y)
         [v kv] (if (< kx ky) [x kx] [y ky])]
     (loop [v v kv kv more more]
       (if more
         (let [w (first more)
               kw (k w)]
           (if (< kw kv)
             (recur w kw (next more))
             (recur v kv (next more))))
         v)))))


;; fastest version of min-key for expensive key calculations
;; but hard to beat the original for keyword access
(defn GOOD-lokey
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (let [rez (r/reduce (fn [res z]
                         (let [kz (k z)]
                           (if (< (peek res) kz) res [z kz])))
                       (let [kx (k x)
                             ky (k y)]
                         (if (< kx ky) [x kx] [y ky]))
                       more)]
     (rez 0))))

(defn lokey
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   ((reduce (fn [res z]
              (let [kz (k z)]
                (if (< (peek res) kz) res [z kz])))
            (let [kx (k x)
                  ky (k y)]
              (if (< kx ky) [x kx] [y ky]))
            more)
    0)))



(defn mktest []
  (dotimes [_ 100]
    (let [k #(Math/tan %)
          rrr (range 100)]
      (flush) (println 'min-key)
      (time (apply min-key k rrr)) 
      (flush) (println 'patch-min-key)
      (time (apply patch-min-key k rrr))
      (flush) (println 'lokey)
      (time (apply lokey k rrr))
      (println))))

;; memoize is a bad idea for this


(defn klokey
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (if (keyword? k)
     (reduce #(lokey k % %2) (lokey k x y) more)
     ((r/reduce (fn [res z]
                         (let [kz (k z)]
                           (if (< (peek res) kz) res [z kz])))
                       (let [kx (k x)
                             ky (k y)]
                         (if (< kx ky) [x kx] [y ky]))
                       more)
       0))))


(defn rkey
   ([k x] x)
   ([k x y] (if (< (k x) (k y)) x y))
   ([k x y & more]
    (r/reduce #(rkey k %1 %2) (rkey k x y) more)))

;; expensive keyfn from JMC  
(defn m91 [n]
  (if (> n 100)
      (- n 10)
	  (m91 (m91 (+ n 11)))))

(defn k91 [n]
  (+ n (m91 n)))
