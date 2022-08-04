(ns eric.roboto)

;;; https://gist.github.com/ericnormand/d6fa214ed0733998a2e0a3f4d3fb62e5

;;; A futuristic robot is programmed to take in a sequence of numbers. Each number is the
;;; distance to travel in a cardinal direction (north, south, east, west). It starts facing
;;; north at (0, 0), travels straight ahead by the distance given in the first number, then
;;; turns 90 degrees clockwise, now facing east. Then it repeats with the next number. Your
;;; job is to calculate where it ends up at the end of the sequence.



(defn smoke-rob [move]
  (assert (= (move [])  [0 0])) ;; No motion
  (assert (= (move [10])  [0 10])) ;; move 10 straight north
  (assert (= (move [10 2])  [2 10]))
  (assert (= (move [10 2 3]) [2 7]))
  (assert (= (move (vec (repeat 40 10))) [0 0]))
  true)

;;; fastest
(defn move [moves]
  (pop (reduce (fn [[east north dir] m]
                 (case dir
                   :north [east (+ north m) :east]
                   :east [(+ east m) north  :south]
                   :south [east (- north m) :west]
                   :west [(- east m) north  :north]))
               [0 0 :north]
               moves)))


;;; nice but not so fast
(defn tmove [moves]
  (let [sum4th (fn [d] (transduce (comp (drop d) (take-nth 4)) + 0 moves))]
    [(- (sum4th 1) (sum4th 3))
     (- (sum4th 0) (sum4th 2))]))


;; SEM hacked we-move solution, faster
(defn we-move2 [moves]
  (loop [east 0 north 0 [n e s w & ms] moves]
    (if n
      (recur (long (+ east (or e 0) (- (or w 0))))
             (long (+ north (or n 0) (- (or s 0))))
             ms)
      [east north])))

;;; intermediate idea on the way from loop to reduce. My final `move` is faster and cleaner
;;; but this was the basis.
(defn lmove [moves]
  (loop [east 0 north 0 dir :north ms moves]
    (if (seq ms)
      (let [m (long (first ms))]
        (case dir
          :north (recur east (+ north m) :east (rest ms))
          :east (recur (+ east m) north  :south (rest ms))
          :south (recur east (- north m) :west (rest ms))
          :west (recur (- east m) north  :north (rest ms))))
      [east north])))




;;; from solutions
(defn we-move [xs]
  (loop [[x y] [0 0] [n e s w & r] (concat xs (repeat 4 0))]
    (if (seq r)
      (recur [(+ x (- e w)) (+ y (- n s))] r)
      [x y])))


(defn we-move1 [xs]
  (reduce (fn [val [x [k f]]] (update val k f x))
          [0 0]
          (map vector xs (cycle [[1 +] [0 +] [1 -] [0 -]]))))



(defn er-move [moves]
  (mapv
    #(reduce + (map * (drop % (cycle [0 1 0 -1])) moves))
    [0 1]))


