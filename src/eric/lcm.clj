(ns eric.lcm)

;;; https://gist.github.com/ericnormand/5c744622c99a1486c90ada14ce9e6c85

;;; See also the old file elcm.clj.  Eric likes to repeat his challenges. :-)

(defn lcm [nums]
  #_ {:pre [(every? pos-int? nums)]}
  (let [gcd (fn [x y]
              (if (zero? y)
                x
                (recur y (rem x y))))]
    (when (seq nums)
      (reduce (fn [x y] (quot (* x y) (gcd x y))) 1 nums))))


(defn lcm2 [nums]
  #_ {:pre [(every? pos-int? nums)]}
  (when (seq nums)
    (let [gcd (fn [x y]
                (if (zero? y)
                  x
                  (recur y (rem x y))))]
      (reduce (fn [x y] (quot (* x y) (gcd x y))) 1 nums))))


(defn gcd2 [x y]
  (if (zero? y)
    x
    (recur y (rem x y))))

(defn lcm3 [nums]
  #_ {:pre [(every? pos-int? nums)]}
  (when (seq nums)
    (reduce (fn [x y] (quot (* x y) (gcd2 x y))) 1 nums)))


(defmacro assert=
  ([] true)
  ([form result & more]
   `(do (assert (= ~form ~result))
        (assert= ~@more))))


(defn smoke-lcm [lcm]
  (assert=
   (lcm [])  nil
   (lcm [10])  10
   (lcm [2 4])  4
   (lcm [3 7])  21
   (lcm [2 4 10])  20
   (lcm [15 2 4])  60
   (lcm (range 1 20)) 232792560))



(defn mc-lcm [[f & r]]
  (loop [i f]
    (if (every? #(zero? (mod i %)) r) i
      (recur (+ i f)))))
