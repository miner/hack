(ns miner.havelhakimi)

;; https://kaygun.tumblr.com/post/637449740678791168/havelhakimi-algorithm-in-clojure

;; https://en.wikipedia.org/wiki/Havel%E2%80%93Hakimi_algorithm

(defn havel-hakimi [xs]
  (loop [ys xs]
    (cond (empty? ys) true
          (odd? (reduce + ys)) false
          :else (recur (->> (repeat (- (count ys) (first ys) 1) 0)
                            (concat (repeat (first ys) 1))
                            (map - (rest ys))
                            (remove #{0})
                            sort)))))


(defn smoke-hh
  ([] (smoke-hh havel-hakimi))
  ([havel-hakimi]
   (assert (havel-hakimi [3 3 3 3]))
   (assert (not (havel-hakimi [1 2 3 4])))
   (assert (havel-hakimi
            [1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 5 5 6]))
   true))


;; don't need to count the 0s at the end since the (map - ...) will stop
;; sort before getting rid of leading 0s
;; use drop-while instead of of scanning whole list for 0

(defn hh [ys]
  (cond (empty? ys) true
        (odd? (reduce + ys)) false
        :else (recur (->> (concat (repeat (first ys) 1) (repeat 0))
                          (map - (rest ys))
                          sort
                          (drop-while zero?)))))

