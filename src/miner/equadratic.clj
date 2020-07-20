(ns miner.equadratic)

;; Quadratic formula
;; https://gist.github.com/ericnormand/46a47b418aaa41604f8d226ee3e9ab09

;; y = ax^2 + bx + c  -- represent as vector of [a b c]
;; roots are where y=0


;; useful info
;; https://courses.lumenlearning.com/boundless-algebra/chapter/graphs-of-quadratic-functions/


;; Oops.  It looks like arg should have been single vector [[a b c]] instead of three-arg that
;; I did.  Close enough.

;; ninjure
(defn ninjure [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (when-not (neg? discriminant)
      (distinct (map #(/ (% (- b) (Math/sqrt discriminant))
                        2 a)
                     [+ -])))))

;; Bobby Towers
(defn btowers [a b c]
  (let [m (* -1/2 (/ b a))
        d (Math/sqrt (- (* m m) (/ c a)))]
    [(+ m d) (- m d)]))


(defn enormand [a b c]
  (let [discr (- (* b b) (* 4 a c))]
    (if (neg? discr)
      #{}
      (let [sd    (/ (Math/sqrt discr) 2 a)
            bpart (/ (- b) 2 a)]
        (conj #{}
              (+ bpart sd)
              (- bpart sd))))))


(defn quad1 [a b c]
  (let [discr (- (* b b) (* 4.0 a c))]
    (when-not (neg? discr)
      (let [drt (Math/sqrt discr)
            a2 (* 2.0 a)]
        (list (/ (- drt b) a2)
              (/ (- (+ b drt)) a2))))))

;; My favorite
(defn quad [a b c]
  (let [discr (- (* b b) (* 4.0 a c))]
    (when-not (neg? discr)
      (let [drt (Math/sqrt discr)
            n2a (* -2.0 a)]
        (list (/ (- b drt) n2a)
              (/ (+ b drt) n2a))))))



;; slightly faster
(set! *unchecked-math* :warn-on-boxed)

;; needs hints
(defn quad7 [^long a ^long b ^long c]
  (let [discr (- (* b b) (* 4.0 a c))]
    (when-not (neg? discr)
      (let [drt (Math/sqrt discr)
            na2 (* -2.0 a)]
        (list (/ (- b drt) na2)
              (/ (+ b drt) na2))))))

(set! *unchecked-math* nil)


(defn round$ [x]
  (long (+ (* 100.0 x) 0.5)))

(defn =$ [a b]
  (= (round$ a) (round$ b)))

(defn eqs [coll1 coll2]
  (every? true? (map =$ (sort coll1) (sort coll2))))

;; slower, but perhaps more accurate on count
(defn eqs1 [coll1 coll2]
  (= (set (map round$ coll1))
     (set (map round$ coll2))))


(defn smoke-quad [quad]
  (assert (eqs (quad 5 6 1) #{-0.2 -1.0}))
  (assert (eqs (quad 2 5 3) #{-1.5 -1.0}))
  (assert (eqs (quad 1 2 -3) #{1.0 -3.0}))
  (assert (eqs (quad 2 -7 3) #{0.5 3.0}))
  (assert (eqs (quad 1 -12 -28) #{14.0 -2.0}))
  (assert (eqs (quad 2 4 2) #{-1.0}))
  true)


;; y = ax^2 + bx + c

;; x-coord of vertext is -b/2a

(defn vertex [^long a ^long b ^long c]
  (let [x (/ b (* -2.0 a))
        y (+ (* a x x) (* b x) c)]
    [x y]))


(defn y-intercept [a b c]
  [0.0 (double c)])


(defn roots->abc
  ([r0] (roots->abc r0 r0))
  ([r1 r2]
   [1.0 (- (+ r1 r2)) (* r1 r2)]))
