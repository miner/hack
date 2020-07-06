(ns miner.equadratic)

;; Quadratic formula
;; https://gist.github.com/ericnormand/46a47b418aaa41604f8d226ee3e9ab09



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


(defn quad [a b c]
  (let [discr (- (* b b) (* 4.0 a c))]
    (when-not (neg? discr)
      (let [drt (Math/sqrt discr)
            n2a (* -2.0 a)]
        (list (/ (- b drt) n2a)
              (/ (+ b drt) n2a))))))



;; slightly faster
(set! *unchecked-math* :warn-on-boxed)

(defn quad7 [^long a ^long b ^long c]
  (let [discr (- (* b b) (* 4.0 a c))]
    (when-not (neg? discr)
      (let [drt (Math/sqrt discr)
            na2 (* -2.0 a)]
        (list (/ (- b drt) na2)
              (/ (+ b drt) na2))))))



(defn smoke-quad [quad]
  (assert (= (set (quad 5 6 1)) #{-0.2 -1.0}))
  (assert (= (set (quad 2 5 3)) #{-1.5 -1.0}))
  (assert (= (set (quad 1 2 -3)) #{1.0 -3.0}))
  (assert (= (set (quad 2 -7 3)) #{0.5 3.0}))
  (assert (= (set (quad 1 -12 -28)) #{14.0 -2.0}))
  (assert (= (set (quad 2 4 2)) #{-1.0}))
  true)

