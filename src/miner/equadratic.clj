(ns miner.equadratic)

;; Quadratic formula
;; https://gist.github.com/ericnormand/46a47b418aaa41604f8d226ee3e9ab09

;; y = ax^2 + bx + c  -- represent as vector of [a b c]
;; roots are where y=0

;; ninjure
(defn quad-ninjure [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (when-not (neg? discriminant)
      (distinct (map #(/ (% (- b) (Math/sqrt discriminant))
                        2 a)
                     [+ -])))))





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


(defn smoke-quad [quad]
  (assert (= (set (quad 5 6 1)) #{-0.2 -1.0}))
  (assert (= (set (quad 2 5 3)) #{-1.5 -1.0}))
  (assert (= (set (quad 1 2 -3)) #{1.0 -3.0}))
  (assert (= (set (quad 2 -7 3)) #{0.5 3.0}))
  (assert (= (set (quad 1 -12 -28)) #{14.0 -2.0}))
  (assert (= (set (quad 2 4 2)) #{-1.0}))
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
