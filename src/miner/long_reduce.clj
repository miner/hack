(ns miner.long-reduce
  (:require [criterium.core :as crit :refer [quick-bench bench]])
  )


(comment ;; for REPL
  (require '[criterium.core :as crit :refer [quick-bench bench]])
  (use 'clj-bench.miner)
  )

(def ys (vec (range 1e6)))
(def ys-large (vec (range 1e7)))


;; rem slightly faster than mod, and same for zero test
(defn zmod? [n d]
  (zero? (rem n d)))

;; SEM: why is (zmod 3) slower than #(zmod? % 3)  Function call overhead? Lack of inline?
(defn zmod [^long d]
  (fn [^long n] (= 0 ^long (rem n d))))

(defn zmod-slow [d]
  #(zmod? % d))





;;; 12/12/17  16:51 by miner --

;; obsolete
(defn la-update! [^longs arr ^long idx f]
  (aset arr idx ^long (f (aget arr idx)))
  arr)

(defn la-reduce
  ([fi xs] (la-reduce fi inc [0] xs))
  ([fi size-or-seq xs] (la-reduce fi inc size-or-seq xs))
  ([fi fup size-or-seq xs]
  ;; init state is long array "size-or-seq"
  ;; fi -- (fn [x]) returns nil for no update, or index to apply fup 
  ;; fup updates arr slot (fup old)
  ;; final result is a vec of longs (not array)
  (vec (reduce (fn [^longs arr x]
                 (when-let [i (fi x)]
                   (aset arr ^long i ^long (fup (aget arr ^long i))))
                 arr)
               (long-array size-or-seq)
               xs))))


(defn aoe2 [xs]
  (la-reduce #(if (odd? %) 0 1) inc [0 0] xs))

(defn aoe4 [xs]
  (let [res (la-reduce #(when (odd? %) 0) inc [0] xs)]
    (conj res (- (count xs) (res 0)))))


(defn aoe5 [xs]
  (let [res (la-reduce #(when (odd? %) 0) xs)]
    (conj res (- (count xs) (res 0)))))

(defn aoe [xs]
  (vec (reduce (fn [^longs tally ^long x] (la-update! tally (if (odd? x) 0 1) inc))
               (long-array 2)
               xs)))

(defn fizzbuzz [n]
  (cond (zmod? n 15) :fizzbuzz
        (zmod? n 5) :buzz
        (zmod? n 3) :fizz
        :else n))

(defn fbi [n]
  (cond (zmod? n 15) 2
        (zmod? n 3)  0
        (zmod? n 5)  1
        :else nil))
  


(defn fbz [xs]
  (zip-map [:fizz :buzz :fizzbuzz] (la-reduce fbi inc 3 xs)))
    


(defn atally-by [pred xs]
  (vec (reduce (fn [^longs tally ^long x] (la-update! tally (if (pred x) 0 1) inc))
               (long-array 2)
               xs)))

(defn cf-tally-by [pred xs]
  (let [total (count xs)
        cnt (count (filter pred xs))]
    [cnt (- total cnt)]))

(defn xtally-by [pred xs]
  (let [total (count xs)
        cnt (transduce (comp (filter pred) (map (constantly 1))) + xs)]
    [cnt (- total cnt)]))

(defn ktally-by [pred xs]
  (let [total (count xs)
        cnt (transduce (keep #(when (pred %) 1)) + xs)]
    [cnt (- total cnt)]))

(defn rtally-by [pred xs]
  (let [total (count xs)
        cnt (reduce (fn [res x] (if (pred x) (inc res) res)) 0 xs)]
    [cnt (- total cnt)]))

(require '[clojure.core.reducers :as r])

;; pretty good
(defn tally-by1 [pred xs]
  (let [total (count xs)
        cnt (r/fold + (r/map (constantly 1) (r/filter pred xs)))]
    [cnt (- total cnt)]))

;; fastest
(defn tally-by [pred xs]
  (let [total (count xs)
        cnt (r/fold + (r/map (fn [_] 1) (r/filter pred xs)))]
    [cnt (- total cnt)]))

;; no good
(defn ty-by [pred xs]
  (let [total (count xs)
        cnt (r/fold + (r/mapcat #(when (pred %) [1]) xs))]
    [cnt (- total cnt)]))

(comment
  (quick-bench (tally-by odd? ys))
  ;;Execution time mean : 4.156724 ms
  
  (quick-bench (tally-by odd? ys-large))
  ;; Execution time mean : 39.681169 ms

  )

;;; 12/12/17  16:51 by miner -- 


;; copied from odd-even-iter in iter.clj, added pred arg
(defn oe-iter [pred xs]
  (let [it (clojure.lang.RT/iter xs)]
    (loop [yes 0
           no 0]
      (if (.hasNext it)
        (if (pred (.next it))
          (recur (inc yes) no)
          (recur yes (inc no)))
        [yes no]))))

;; slightly better
(defn one-iter [pred xs]
  (let [it (clojure.lang.RT/iter xs)]
    (loop [yes 0]
      (if (.hasNext it)
        (if (pred (.next it))
          (recur (inc yes))
          (recur yes))
        [yes (- (count xs) yes)]))))

;; somewhat better
(defn two-iter [pred xs]
  (let [it (clojure.lang.RT/iter xs)]
    (loop [yes 0]
      (if (.hasNext it)
        (recur (if (pred (.next it)) (inc yes) yes))
        [yes (- (count xs) yes)]))))

;; not better
(defn iter3 [pred xs]
  (let [cnt (count xs)]
    (loop [i (dec cnt) yes 0]
      (if (neg? i)
        [yes (- cnt yes)]
        (recur (dec i) (if (pred (xs i)) (inc yes) yes))))))

