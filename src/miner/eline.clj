(ns miner.eline)

;; https://gist.github.com/ericnormand/81722b80fc7d0b2972fda68652489f65

;; parallel lines
;; ax + by = c
;; as vector [a b c]

;; para is same slope, any y-intercept
;; y = (-a/b)x + c/b

;; question: are identical lines parallel?  I say yes.  Eric said no.
;; Other sources say two lines can be "coincident" (identical, but perhaps different
;; equations), parallel, or intersecting (at one point).  The parallel definition boils down
;; to same slope and different y-intercept.

;; b zeroes are dangerous for division
;; (= 0 a b) is illegal (not a line), but should we test?  Probably.


;; faster to use multiplication (crossways).  Avoids issues with zeroes.
;; Eric's non-coincident parallel
;; must check y-intercept
(defn parallel? [[a b c] [a2 b2 c2]]
  {:pre [(not= 0 a b)
         (not= 0 a2 b2)
         (every? int? [a b c a2 b2 c2])]}
  (and (= (* a b2) (* a2 b))
       (not= (* c b2) (* c2 b))))




;; OLD
;; direct definition
;; be carful about zeroes
(defn para? [[a b c] [a2 b2 c2]]
  (if (or (zero? b) (zero? b2))
    (and (zero? b)
         (zero? b2)
         (not= c c2))
    (and (= (/ a b) (/ a2 b2))
         (not= (/ c b) (/ c2 b2)))))
  




;; I personally think identical "coincident" lines should be considered parallel for
;; convenience.  That would allow just a slope check.  But that's not what Eric asked for.


(defn slope= [[a b c] [a2 b2 c2]]
  (= (* a b2) (* a2 b)))

;; check for illegal lines
(defn slope=2? [[a b c] [a2 b2 c2]]
  {:pre [(not= 0 a b)
         (not= 0 a2 b2)
         (every? int? [a b c a2 b2 c2])]}
       (= (* a b2) (* a2 b)))




(defn smoke-para [p?]
  (assert (p? [2 4 1] [1 2 1]))
  (assert (p? [2 4 1] [1 2 11]))
  (assert (not (p? [2 4 1] [4 2 1])))
  (assert (p? [1 5 1] [1 5 10]))
  (assert (not (p? [0 1 5] [0 1 5])))
  (assert (not (p? [1 5 1] [2 10 2])))
  (assert (not (p? [1 0 0] [2 0 0])))
  (assert (not (p? [1 0 0] [0 2 0])))
  true)




