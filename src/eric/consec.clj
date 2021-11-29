(ns eric.consec)

;;; https://gist.github.com/ericnormand/6cc9a64238b4929510bb3d3025d60151

;; Consecutive numbers
;; 
;; Write a function that determines whether a sequence of integers can be rearranged into a
;; sequence of consecutive numbers without duplicates. The function should return the sequence
;; of consecutive numbers or nil if it is not possible.


;; fastest and clever reduce
(defn consec [coll]
  (let [sss (sort coll)]
    (when (reduce (fn ([r x] (if (= (inc r) x) x (reduced false))) ([] true)) sss)
        sss)))




;;; not as fast as reduce
(defn consec7 [coll]
  (let [sss (sort coll)]
    (loop [r (first sss) xs (next sss)]
      (cond (nil? xs) sss
            (not= (inc r) (first xs)) nil
            :else (recur (first xs) (next xs))))))



(defn consec11 [coll]
  (let [sss (sort coll)]
    (when (reduce (fn ([] true) ([r x] (if (= (inc r) x) x (reduced false)))) sss)
        sss)))





(defn consec1 [coll]
  (let [sss (sort coll)]
    (when (every? true? (map = sss (iterate inc (first sss))))
      sss)))

(defn consec2 [coll]
 (let [sss (sort coll)]
    (when (= sss (take (count coll) (iterate inc (first sss))))
      sss)))


(defn consec-OK [coll]
  (if (empty? coll)
    coll
    (let [sss (sort coll)]
      (when (reduce (fn [r x] (if (= r x) (inc x) (reduced nil))) (first sss) sss)
        sss))))

;;; not faster
(defn consec3 [coll]
  (if (empty? coll)
    coll
    (let [sss (sort coll)]
      (when (reduce (fn [r x] (if (= r x) (inc r) (reduced nil))) (inc (first sss)) (rest sss))
        sss))))

;; a bit faster, not as clear
(defn consec5 [coll]
  (let [sss (sort coll)]
    (when (reduce (fn [r x] (if (= r x) (inc x) (reduced nil))) (or (first sss) coll) sss)
      sss)))








(defn smoke-consec [consec]
  (assert (= (consec []) ())) ;; trivially true
  (assert (= (consec [1]) '(1)))
  (assert (= (consec [3 1 2]) '(1 2 3)))
  (assert (= (consec [5 3 2 1]) nil)) ;; non-consecutive (4 is missing)
  (assert (= (consec [7 8 9 7]) nil)) ;; 7 repeats
  (assert (= (consec (range 100)) (range 100)))
  true)

