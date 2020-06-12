(ns miner.knuthmb)

;;; Knuth's "Man or Boy" compiler test
;;; https://en.wikipedia.org/wiki/Man_or_boy_test


;;; Algol 60

;; begin
;;   real procedure A(k, x1, x2, x3, x4, x5);
;;   value k; integer k;
;;   real x1, x2, x3, x4, x5;
;;   begin
;;     real procedure B;
;;     begin k := k - 1;
;;           B := A := A(k, B, x1, x2, x3, x4)
;;     end;
;;     if k ≤ 0 then A := x4 + x5 else B
;;   end
;;   outreal(1,A(10, 1, -1, -1, 1, 0))
;; end


;;; my best, fast enough
;;; k int, ka atom holding int
;;; xN are nullary functions so they can be invoked like B
;;; alternative is to test fn? when adding, but that's slower
(defn kmb
  ([] (kmb 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (if-not (pos? k)
               (+ (x4) (x5))
               (let [k (atom k)]
                 (letfn [(B [] (A (swap! k dec) B x1 x2 x3 x4))]
                   (B)))))]
     (let [c1 (constantly 1)
           c-1 (constantly -1)
           c0 (constantly 0)]
       (A k c1 c-1 c-1 c1 c0)))))



(defn kmb008
  ([] (kmb008 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (let [ka (atom k)]
               (letfn [(B [] (A (swap! ka dec) B x1 x2 x3 x4))]
                 (if-not (pos? k)
                   (+ (x4) (x5))
                   (B)))))]
     (let [c1 (constantly 1)
           c-1 (constantly -1)
           c0 (constantly 0)]
       (A k c1 c-1 c-1 c1 c0)))))






(defn kmb77
  ([] (kmb77 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (let [k (atom k)]
               (letfn [(B [] (A (swap! k dec) B x1 x2 x3 x4))]
                 (if-not (pos? @k)
                   (+ (x4) (x5))
                   (B)))))]
     (let [c1 (constantly 1)
           c-1 (constantly -1)
           c0 (constantly 0)]
       (A k c1 c-1 c-1 c1 c0)))))




;; slower
(defn kmb2
  ([] (kmb2 10))
  ([k]
   (let [fval (fn [x] (if (fn? x) x (constantly x)))]
     (letfn [(A [k x1 x2 x3 x4 x5]
               (let [k (atom k)
                     x4 (fval x4)
                     x5 (fval x5)]
                 (letfn [(B []
                           (swap! k dec)
                           (A @k B x1 x2 x3 x4))]
                   (if-not (pos? @k)
                     (+ (x4) (x5))
                     (B)))))]
       (A k 1 -1 -1 1 0)))))

(defn kmb11
  ([] (kmb11 10))
  ([k]
   (let [f+ (fn [x y] (+ (if (fn? x) (x) x)
                         (if (fn? y) (y) y)))]
     (letfn [(A [k x1 x2 x3 x4 x5]
               (let [k (atom k)]
                 (letfn [(B []
                           (swap! k dec)
                           (A @k B x1 x2 x3 x4))]
                   (if-not (pos? @k)
                     (f+ x4 x5)
                     (B)))))]
       (A k 1 -1 -1 1 0)))))


;;; good, faster
(defn kmb12
  ([] (kmb12 10))
  ([k]
   (let [f+ (fn [x y] (+ (if (fn? x) (x) x)
                         (if (fn? y) (y) y)))]
     (letfn [(A [k x1 x2 x3 x4 x5]
               (let [ka (atom k)]
                 (letfn [(B [] (A (swap! ka dec) B x1 x2 x3 x4))]
                   (if-not (pos? @ka)
                     (f+ x4 x5)
                     (B)))))]
       (A k 1 -1 -1 1 0)))))



(defn kmb13
  ([] (kmb13 10))
  ([k]
   (let [f+ (fn [x y] (+ (if (fn? x) (x) x)
                         (if (fn? y) (y) y)))]
     (letfn [(A [k x1 x2 x3 x4 x5]
               (let [ka (atom k)]
                 (letfn [(B [] (A (swap! ka dec) B x1 x2 x3 x4))]
                   (if-not (pos? k)
                     (f+ x4 x5)
                     (B)))))]
       (A k 1 -1 -1 1 0)))))





;; pretty good
(defn kmb1
  ([] (kmb1 10))
  ([k]
   (let [fval (fn [x] (if (fn? x) (x) x))]
     (letfn [(A [k x1 x2 x3 x4 x5]
               (let [k (atom k)]
                 (letfn [(B []
                           (swap! k dec)
                           (A @k B x1 x2 x3 x4))]
                   (if-not (pos? @k)
                     (+ (fval x4) (fval x5))
                     (B)))))]
       (A k 1 -1 -1 1 0)))))





(defn kmb0
  ([] (kmb0 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (let [k (atom @k)]
               (letfn [(B []
                         (swap! k dec)
                         (A k B x1 x2 x3 x4))]
                 (if-not (pos? @k)
                   (+ (x4) (x5))
                   (B)))))]
     (let [c1 (constantly 1)
           c-1 (constantly -1)
           c0 (constantly 0)]
       (A (atom k) c1 c-1 c-1 c1 c0)))))





;;; More answers at link
;; k	A(k,1,-1,-1,1,0)
;; 0	1
;; 1	0
;; 2	−2
;; 3	0
;; 4	1
;; 5	0
;; 6	1
;; 7	−1
;; 8	−10
;; 9	−30
;; 10	−67
;; 11	−138
;; 12	−291


(defn smoke-kmb
  ([] (smoke-kmb kmb))
  ([kmb]
  (assert (= (kmb 10) -67))
  (assert (= (kmb 0) 1))
  (assert (= (kmb 1) 0))
   (assert (= (kmb 2) -2))
   (assert (= '(1 0 -2 0 1 0 1 -1 -10 -30 -67 -138)
              (map kmb (range 12))))
  true))


;;; http://www.chilton-computing.org.uk/acl/applications/algol/p006.htm
;;; Knuth later figured out:
;;; A(k, xl, x2, x3, x4, x5) is equal to cl × xl + c2 × x2 + c3 × x3 + c4 × x4 + c5 × x5
;;; where the coefficients are given in the following table:

;;  k     c1(k)    c2(k)    c3(k)   c4(k)   c5(k)
;;  0      0        0        0       1       1
;;  1      0        0        1       1       0
;;  2      0        1        1       0       0
;;  3      1        1        0       0       0
;;  4      2        1        0       0       0
;;  5      3        2        1       0       0
;;  6      5        3        3       2       0
;;  7      8        6        9       6       0
;;  8     14       15       22      13       0
;;  9     29       37       48      26       0
;; 10     66       85      102      54       0







;;; Rosetta Code solution
;;; https://rosettacode.org/wiki/Man_or_boy_test#Clojure
(declare a)
 
(defn man-or-boy
  "Man or boy test for Clojure"
  [k]
  (let [k (atom k)]
    (a k
       (fn [] 1)
       (fn [] -1)
       (fn [] -1)
       (fn [] 1)
       (fn [] 0))))
 
(defn a
  [k x1 x2 x3 x4 x5]
  (let [k (atom @k)]
    (letfn [(b []
               (swap! k dec)
               (a k b x1 x2 x3 x4))]
      (if (<= @k 0)
        (+ (x4) (x5))
        (b)))))
 
#_ (man-or-boy 10)

                      
