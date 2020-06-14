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
;;; for A, k is int, then wrapped in atom for B
;;; The atom is necessary to allow later modification when B is invoked later.
;;; xN are nullary functions so they can be invoked like B
;;; alternative is to test fn?/int? when adding, but that's slower
;;; NB: stack overflow on k=15
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



(defn kmb52
  ([] (kmb52 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (if-not (pos? k)
               (+ (x4) (x5))
               (let [k (atom k)]
                 (letfn [(B [] (A (swap! k dec) B x1 x2 x3 x4))]
                   (B)))))]
     (let [c1 (fn [] 1)
           c-1 (fn [] -1)
           c0 (fn [] 0)]
       (A k c1 c-1 c-1 c1 c0)))))



(defn pkmb
  ([] (pkmb 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (if-not (pos? k)
               (let [aret (+ (x4) (x5))]
                 (println "A ret" aret "  k=" k)
                 aret)
               (let [k (atom k)]
                 (letfn [(B []
                           (println " B" @k (str k))
                           (A (swap! k dec) B x1 x2 x3 x4))]
                   (B)))))]
     (let [c1 (fn [] 1)
           c-1 (fn [] -1)
           c0 (fn [] 0)]
       (A k c1 c-1 c-1 c1 c0)))))





;; slower
(defn kmb2
  ([] (kmb2 10))
  ([k]
   (letfn [(A [k x1 x2 x3 x4 x5]
             (if-not (pos? k)
               (+ (x4) (x5))
               (let [k (atom k)]
                 (letfn [(B [] (A (swap! k dec) B x1 x2 x3 x4))]
                   (B)))))]
     ;; convert constants to nullary fns
     (let [A (fn [k & args] (apply A k (map constantly args)))]
       (A k 1 -1 -1 1 0)))))


;; slower
(defn kmb3
  ([] (kmb3 10))
  ([k]
   (let [fval (fn [x] (if (fn? x) (x) x))]
     (letfn [(A [k x1 x2 x3 x4 x5]
               (if-not (pos? k)
                 (+ (fval x4) (fval x5))
                 (let [k (atom k)]
                   (letfn [(B [] (A (swap! k dec) B x1 x2 x3 x4))]
                     (B)))))]
       (A k 1 -1 -1 1 0)))))




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
   (assert (= [1 0 -2 0 1 0 1 -1 -10 -30 -67 -138]
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




;;; The On-Line Encyclopedia of Integer Sequences
;;; https://oeis.org/A132343

;; Markus Jarderot
;; mizardx@gmail.com
;; Jun 05 2010

;; formula:
;; a(5)=0, a(6)=1, a(7)=-1, a(8)=-10, a(9)=-30,
;; a(n)=a(n-5)-6*a(n-4)+11*a(n-3)-10*a(n-2)+5*a(n-1) for n >= 10

(defn fmb [^long k]
  (case k
    0 1
    1 0
    2 -2
    3 0
    4 1
    5 0
    6 1
    7 -1
    8 -10
    9 -30
    (+ (fmb (- k 5))
       (* -6 (fmb (- k 4)))
       (* 11 (fmb (- k 3)))
       (* -10 (fmb (- k 2)))
       (* 5 (fmb (dec k))))))

;; good if you want sequence
(defn mbseq [n]
  (let [init [1 0 -2 0 1 0 1 -1 -10 -30]
        cnt (count init)
        step (fn [vk]
               (let [k (count vk)]
                 (conj vk (+ (vk (- k 5))
                             (* -6 (vk (- k 4)))
                             (* 11 (vk (- k 3)))
                             (* -10 (vk (- k 2)))
                             (* 5 (vk (dec k)))))))]
    (if (< n cnt)
      (subvec init 0 n)
      (loop [vk init need (- n cnt)]
        (if (pos? need)
          (recur (step vk) (dec need))
          vk)))))




;; fast for single k but fmb is faster???
(defn mjmb [kth]
  (let [init [1 0 -2 0 1 0 1 -1 -10 -30]
        step (fn [vk]
               (let [k (count vk)]
                 (conj vk (+ (vk (- k 5))
                             (* -6 (vk (- k 4)))
                             (* 11 (vk (- k 3)))
                             (* -10 (vk (- k 2)))
                             (* 5 (vk (dec k)))))))]
    (or (nth init kth nil)
        (loop [vk init need (- (inc kth) (count init))]
          (if (pos? need)
            (recur (step vk) (dec need))
            (peek vk))))))

;; see also fib.clj for performance tricks
;; (def cgfibs (map first (iterate (fn [[^long a ^long b]] [b (+ a b)]) [0 1])))

(def infmbs (concat [1 0 -2 0 1] 
                    (map first (iterate
                                (fn [prev5]
                                  (conj (subvec prev5 1)
                                        (+ (prev5 0)
                                           (* -6 (prev5 1))
                                           (* 11 (prev5 2))
                                           (* -10 (prev5 3))
                                           (* 5 (prev5 4)))))
                                [0 1 -1 -10 -30]))))



;;; Rosetta Code solution
;;; https://rosettacode.org/wiki/Man_or_boy_test#Clojure
;;; SEM: mine is better.  Less atoms and faster.
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

                      
