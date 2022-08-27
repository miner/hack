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
   (lcm (range 1 20)) 232792560
   (lcm (range 19 1 -1)) 232792560))


;; mc slow
(defn mc-lcm [[f & r]]
  (loop [i f]
    (if (every? #(zero? (mod i %)) r) i
      (recur (+ i f)))))


;; @jonasseglare   but doesn't agree with me about (range 1 20)  BUG!
(defn jo-lcm-bug [numbers]
  (if (seq numbers)
    (transduce (filter #(< 1 %))
               (completing #(denominator (+ (/ 1 %1) (/ 1 %2))))
               1
               numbers)))

;;; SEM fixed but still not so fast

(defn jo2-lcm [numbers]
  (when (seq numbers)
    (reduce (fn [r x]
              (if (= x 1)
                r
                (max (denominator (+ (/ r) (/ x))) r)))
            1
            numbers)))


(defn jo21-lcm [numbers]
  (let [denom (fn [x] (if (ratio? x) (denominator x) 1))]
    (when (seq numbers)
    (reduce (fn [r x]
              (let [d (denom (+ (/ r) (/ x)))]
                (max d r)))
            1
            numbers))))


(defn jo22-lcm [numbers]
  (when (seq numbers)
    (reduce (fn [r x]
              (let [invsum (+ (/ r) (/ x))]
                (if (ratio? invsum)
                  (max (denominator invsum) r)
                  r)))
            1
            numbers)))

(defn jo23-lcm [numbers]
  (when (seq numbers)
    (reductions (fn [r x]
              (let [invsum (+ (/ r) (/ x))]
                (if (ratio? invsum)
                  (max (denominator invsum) r)
                  r)))
            1
            numbers)))

(defn jo24-lcm [numbers]
  (when (seq numbers)
    (reductions (fn [r x]
              (let [invsum (+ (/ r) (/ x))]
                (if (ratio? invsum)
                  (denominator invsum)
                  r)))
            1
            numbers)))

(defn jo25-lcm [numbers]
  (when (seq numbers)
    (reduce (fn [r x]
              (let [invsum (+ (/ r) (/ x))]
                (if (ratio? invsum)
                  (max (denominator invsum) r)
                  r)))
            1
            numbers)))



(defn jo3-lcm [numbers]
  (when (seq numbers)
    (transduce (filter #(> % 1))
               (fn ([r x]
                 (let [d (denominator (+ (/ r) (/ x)))]
                   (max d r)))
                 ([r] r))
               1
               numbers)))

;; not faster
(defn jo4-lcm [numbers]
  (let [denom (fn [x] (if (ratio? x) (denominator x) 1))]
    (when (seq numbers)
      (transduce (comp (map /))
               (fn ([r invx]
                    (let [d (denom (+ r invx))]
                      (if (> d (denom r)) (/ d) r)))
                 ([r] (/ r)))
               1
               numbers))))

(defn BUG_dlcm [nums]
  (when (seq nums)
    (let [sum (reduce + 1 (map / nums))]
      (if (ratio? sum)
        (denominator sum)
        (reduce max nums)))))

(defn BUG2_dlcm [nums]
  (when (seq nums)
    (transduce (comp (filter #(> % 1)) (map /))
               (fn ([r x]
                    (let [sum (+ r x)]
                      (if (ratio? sum)
                        sum
                        r)))
                 ([r] (denominator r)))
               0
               nums)))

(defn dlcm [nums]
  (when (seq nums)
    (loop [ratv (into [] (comp (filter #(> % 1)) (map /)) nums)]
      (if (= (count ratv) 1)
        (denominator (ratv 0))
        (recur (into [] (comp (partitionv-all 2) (map #(reduce + %))) ratv))))))

(defn so-lcm
  [vs]
  (some
    (fn [n]
      (when (every?
              #(zero? (mod n %))
              vs)
        n))
    (range 2 (inc (apply * vs)))))
