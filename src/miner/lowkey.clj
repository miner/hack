;; experiments with CLJ-99

(ns miner.lowkey
  (:require [clojure.core.reducers :as r]
            [criterium.core :as crit]))

(defn orig-min-key
   "Returns the x for which (k x), a number, is least."
   ([k x] x)
   ([k x y] (if (< (k x) (k y)) x y))
   ([k x y & more]
      (reduce #(orig-min-key k %1 %2) (orig-min-key k x y) more)))

(defn patch-min-key1
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

;; patch-min-key1 changed what happened on ties.  Originally, the last won.  patch1 chose
;; last.  Updated patch, goes back to old behavior.  None of this was documented, but some
;; users accidentally depended on it.  I wonder what kind of spec and unit test could have
;; caught this.  I guess you need to preserve the old code and run a generative test against it.

(defn patch-min-key
   "Returns the x for which (k x), a number, is least."
   ([k x] x)
   ([k x y] (if (< (k x) (k y)) x y))
   ([k x y & more]
   (let [kx (k x) ky (k y)
         [v kv] (if (<= kx ky) [x kx] [y ky])]
     (loop [v v kv kv more more]
       (if more
         (let [w (first more)
               kw (k w)]
           (if (<= kw kv)
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


;; NO -- use criterium instead of this
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

(defn gentest
  ([] (gentest second))
  ([kf] (gentest kf 10000))
  ([kf n]
   (let [data (map list (range n) (cycle [-1 -2 -3 -4 -5]))]
     (println "key fn" (str kf) (take 10 data) "...")
     (doseq [mk [orig-min-key min-key patch-min-key patch-min-key1 lokey]]
       (println "\n" (str mk) (apply mk kf data))
       (crit/quick-bench (apply mk kf data))))))


(defn tantest []
  (gentest #(Math/tan (first %)) 10000))


(defn idtest []
  (gentest first 10000))

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
