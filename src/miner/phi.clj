(ns miner.phi)




(def ^:const phi (/ (+ 1.0 (Math/sqrt 5.0)) 2.0))


;; https://en.wikipedia.org/wiki/Continued_fraction

(defn cont
  "Calculate continued fraction with X seed (defautl 1.0), to recursive DEPTH (defautl 40). Phi is
  default result."
  ([] (cont 40 1.0))
  ([x] (cont 40 (double x)))
  ([depth x]
     (if (zero? depth)
       (/ x)
       (+ x (/ (cont (dec depth) x))))))

;; accumulator to avoid StackOverflow
(defn contfrac
  ([] (contfrac 40 1.0 1.0))
  ([x] (let [x (double x)] (contfrac 40 x (/ x))))
  ([depth x] (let [x (double x)] (contfrac depth x (/ x))))
  ([depth x result]
   (if (zero? depth)
     result
     (recur (dec depth) x (+ x (/ result))))))


