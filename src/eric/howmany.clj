(ns eric.howmany
  (:require [clojure.math :as m]))


;;; https://gist.github.com/ericnormand/7174aaccc71025de86ddac77553f8595

;;; Imagine you took all the integers between n and m (exclusive, n < m) and concatenated
;;; them together. How many digits would you have? Write a function that takes two numbers
;;; and returns how many digits. Note that the numbers can get very big, so it is not
;;; possible to build the string in the general case.


(require '[clojure.math :as m])

;;; slightly updated after submission
(defn fast-num-digits [n m]
  #_ (assert (< n m))
  (let [n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [width (long (m/ceil (m/log10 (inc n))))
             p10w (long (m/pow 10.0 width))
             cnt (* (- p10w n) width)]
        (if (< p10w m)
          (recur (inc width) (* p10w 10) (+ cnt (* (inc width) p10w 9)))
          (- cnt (* (- p10w m) width)))))))



;;; slower (2x)
(defn fnd5 [n m]
  #_ (assert (< n m))
  (let [n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (let [www (long (m/ceil (m/log10 (inc n))))]
        (loop [p10w (long (m/pow 10.0 www))
               cnt (* (- p10w n) www)]
          (let [width (long (m/log10 p10w))]
            (if (< p10w m)
              (recur (* p10w 10) (+ cnt (* (inc width) p10w 9)))
              (- cnt (* (- p10w m) width)))))))))




;;; works but not fast enough (4x)
(defn fnd2 [n m]
  #_ (assert (< n m))
  (let [n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (let [calc-width (fn [i] (long (m/ceil (m/log10 (inc i)))))
            p10w (fn [w] (long (m/pow 10.0 w)))
            start-width (calc-width n)
            end-width (calc-width m)]
        (transduce (map-indexed (fn [i w] (* (p10w w) 9 (+ (inc i) start-width))))
                   (completing + (fn [cnt] (- cnt (* (- (p10w end-width) m) end-width))))
                   (* (- (p10w start-width) n) start-width)
                   (range start-width end-width))))))

;;; not good enough
(defn fnd3 [n m]
  #_ (assert (< n m))
  (let [n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (let [calc-width (fn [i] (m/ceil (m/log10 (inc i))))
            p10w (fn [w] (m/pow 10.0 w))
            start-width (calc-width n)
            end-width (calc-width m)]
        (transduce (map-indexed (fn [i w] (* (p10w w) 9 (+ (inc i) start-width))))
                   (completing + (fn [cnt] (long (- cnt (* (- (p10w end-width) m) end-width)))))
                   (* (- (p10w start-width) n) start-width)
                   (range start-width end-width))))))




(defn num-digits [n m]
  ;; (assert (< n m))
  (reduce + (map #(count (str %)) (range (inc n) m))))

;; slightly faster
(defn num-digits1 [n m]
  ;; (assert (< n m))
  (reduce + (map #(.length (str %)) (range (inc n) m))))


(defn num-digits2 [n m]
  ;; (assert (< n m))
  (transduce (comp (map str) (map count))
             +
             0
             (range (inc n) m)))

;; slow
(defn num-digits3 [n m]
  ;; (assert (< n m))
  (transduce (map #(long (inc (m/log10 %))))
             +
             0
             (range (inc n) m)))

;; even slower
(defn num-digits4 [n m]
  ;; (assert (< n m))
  (transduce (comp (map m/log10) (map inc) (map long))
             +
             0
             (range (inc n) m)))

;; new idea -- just consider end points and infer ranges


(defmacro assert=
  ([] true)
  ([form result & more]
   `(do (assert (= ~form ~result))
        (assert= ~@more))))

(defn smoke-nd [num-digits]
  (assert=
   (num-digits 0 1) 0
   ;; (there are no integers between 0 and 1)
   (num-digits 0 10) 9
   ;; (1, 2, 3, 4, 5, 6, 7, 8, 9)
   (num-digits 9 100) 180
   ;; correction from original: 180, not 179
   (num-digits 9 101) 183
   (num-digits 6 6666) 25547
   (num-digits 99 9999) 38696))




(defn ncnt [n]
  (if (zero? n)
    1
    (long (inc (m/log10 n)))))

;;; check edge cases around 0 and 1
(defn pow10-ceil [n]
  (m/pow 10 (m/ceil (m/log10 (inc n)))))

(defn pow10-floor [n]
  (m/pow 10 (m/floor (m/log10 n))))


;; next highest power of 10
(defn pow10-cap [n]
  ;; (assert (not (neg? n)))
  (if (< n 10)
    10
    (long (m/pow 10 (m/ceil (m/log10 (inc n)))))))

;; not sure FIXME
(defn pow10-below [n]
  ;; (assert (not (neg? n)))
  (if (< n 10)
    -1
    (long (m/pow 10 (m/floor (m/log10 n))))))


;;; inclusive N, exclusive next power of 10
(defn count-digits-up10x [n]
  ;; (assert (not (neg? n)))
  (if (< n 10)
    (- 10 n)
    (let [d (m/ceil (m/log10 (inc n)))]
      (* (- (long (m/pow 10 d)) n) (long d)))))

;; inclusive n, inclusive down to lower power of 10
(defn count-digits-down10 [n]
  ;; (assert (not (neg? n)))
  (if (< n 10)
    (- 10 n)
    (let [d (m/ceil (m/log10 (inc n)))]
      (* (- (long (m/pow 10 d)) n) (long d)))))


(defn same-power? [n m]
  ;; (assert (> m n))
  (or (and (< n 10) (< m 10))
      (< (- (m/log10 m) (m/log10 n)) 1.0)))


(defn count-digits-power10 [p]
  (if (zero? p)
    10
    (* (inc p) (long (- (m/pow 10 (inc p)) (m/pow 10 p))))))
  
(defn next-power10 [n]
  (if (zero? n)
    10
    (long (m/pow 10 (m/ceil (m/log10 (inc n)))))))




;; should loop with explicit power p
;; SAVE
(defn numd5 [n m]
  (let [n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [p (next-power10 n) sum (count-digits-up10x n)]
        (cond (= p m) sum
              (< p m) (recur (next-power10 p) (+ sum (count-digits-power10 (long (m/log10 p)))))
              :else (- sum (count-digits-up10x m)))))))

(defn numd51 [n m]
  (let [count-digits-up10x (fn [n]
                             (let [d (m/ceil (m/log10 (inc n)))]
                               (* (- (long (m/pow 10.0 d)) n) (long d))))
        count-digits-power10 (fn [p]
                               (long (* (inc p) (- (m/pow 10.0 (inc p)) (m/pow 10.0 p)))))
        n (inc n)]        
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [p (next-power10 n) cnt (count-digits-up10x n)]
        (cond (= p m) cnt
              (< p m) (recur (next-power10 p) (+ cnt (count-digits-power10 (m/log10 p))))
              :else (- cnt (count-digits-up10x m)))))))


(defn numd53 [n m]
  (let [count-digits-up10x (fn [n]
                             (let [d (m/ceil (m/log10 (inc n)))]
                               (* (- (long (m/pow 10.0 d)) n) (long d))))
        count-digits-power10 (fn [p]
                               (long (* (inc p) (- (m/pow 10.0 (inc p)) (m/pow 10.0 p)))))
        n (inc n)]        
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [p (long (m/pow 10.0 (m/ceil (m/log10 (inc n))))) cnt (count-digits-up10x n)]
        (cond (= p m) cnt
              (< p m) (recur (* p 10) (+ cnt (count-digits-power10 (m/log10 p))))
              :else (- cnt (count-digits-up10x m)))))))

;;; second fastest so far
(defn numd54 [n m]
  (let [log10-ceil (fn [x] (m/ceil (m/log10 (inc x))))
        count-digits-up10x (fn [x]
                             (let [d (log10-ceil x)]
                               (long (* (- (m/pow 10.0 d) x) d))))
        count-digits-power10 (fn [p]
                               (long (* (inc p) (- (m/pow 10.0 (inc p)) (m/pow 10.0 p)))))
        n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [x (log10-ceil (double n)) p (long (m/pow 10.0 x)) cnt (count-digits-up10x n)]
        (cond (= p m) cnt
              (< p m) (recur (inc x) (* p 10) (+ cnt (count-digits-power10 x)))
              :else (- cnt (count-digits-up10x m)))))))


;; was fastest
(defn numd55 [n m]
  (let [log10-ceil (fn [x] (m/ceil (m/log10 (inc x))))
        count-digits-up10x (fn [x]
                             (let [d (log10-ceil x)]
                               (long (* (- (m/pow 10.0 d) x) d))))
        count-digits-power10 (fn [p x10p]
                               (* (inc p) (* x10p 9)))
        n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [x (long (log10-ceil (double n))) x10p (long (m/pow 10.0 x)) cnt (count-digits-up10x n)]
        (cond (= x10p m) cnt
              (< x10p m) (recur (inc x) (* x10p 10) (+ cnt (count-digits-power10 x x10p)))
              :else (- cnt (count-digits-up10x m)))))))

;;; much faster!
(defn numd56 [n m]
  (let [count-digits-up10x (fn [i p10x d] (* (- p10x i) d))
        count-digits-power10 (fn [x p10x] (* (inc x) (* p10x 9)))
        n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [x (long (m/ceil (m/log10 (inc n))))
             p10x (long (m/pow 10.0 (double x)))
             cnt (count-digits-up10x n p10x x)]
        (cond (= p10x m) cnt
              (< p10x m) (recur (inc x) (* p10x 10) (+ cnt (count-digits-power10 x p10x)))
              :else (- cnt (count-digits-up10x m p10x x)))))))


;; faster fastest
(defn numd58 [n m]
  (let [n (inc n)]
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [x (long (m/ceil (m/log10 (inc n))))
             p10x (long (m/pow 10.0 x))
             cnt (* (- p10x n) x)]
        (cond (= p10x m) cnt
              (< p10x m) (recur (inc x) (* p10x 10) (+ cnt (* (inc x) (* p10x 9))))
              :else (- cnt (* (- p10x m) x)))))))







;; slightly slower than 51
(defn numd52 [n m]
  (let [count-digits-up10x (fn [n]
                             (let [d (m/ceil (m/log10 (inc n)))]
                               (long (* (- (m/pow 10.0 d) n) d))))
        count-digits-power10 (fn [p]
                               (long (* (inc p) (- (m/pow 10.0 (inc p)) (m/pow 10.0 p)))))
        n (inc n)]        
    (if (and (< n 10) (<= m 10))
      (- m n)
      (loop [p (next-power10 n) cnt (count-digits-up10x n)]
        (cond (= p m) cnt
              (< p m) (recur (next-power10 p) (+ cnt (count-digits-power10 (m/log10 p))))
              :else (- cnt (count-digits-up10x m)))))))



;; slightly hacked by SEM, not bad, but I like mine better
(defn ju-num-digits [n m]
  #_ {:pre [(>= n 0) ; !! not at all assumed in challenge
         (< n m)]}
  (loop [x (inc n)
         sum 0
         length (count (str x))]
    (if (>= x m) 
      sum
      (let [delta (long (- (min m (m/pow 10.0 length)) x))]
        (recur (+ x delta)
               (+ sum (* length delta))
               (inc length))))))
