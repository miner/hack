(ns eric.sortwith)

;; https://gist.github.com/ericnormand/0efb967277eed772f2a0dda801927375

;;; The sort function sorts a collection based on its "natural ordering." sort-by allows you
;;; to sort a collection based on the natural ordering of the result of some function
;;; applied to the elements. Your task is to write a function sort-with which takes a
;;; sequence of elements that define the sort order.

;;; better with transient, not necessarily transduce
(defn og-sort-with [ordering coll]
  (sort-by (reduce (fn [res x] (assoc! res x (count res))) (transient {}) ordering) coll))


;;; slightly better on larger collections with transient
(defn sort-with [ordering coll]
  (transduce identity
             (fn ([res x] (assoc! res x (count res)))
                 ([res] (sort-by res coll)))
             (transient {})
             ordering))

;; transducer version
(defn xsort-with [ordering coll]
  (transduce identity
             (fn ([res x] (assoc res x (count res)))
                 ([res] (sort-by res coll)))
             {}
             ordering))

;; my first attempt
(defn original-sort-with [ordering coll]
  (sort-by (reduce (fn [res x] (assoc res x (count res))) {} ordering) coll))



(defn smoke-sort [sort-with]
  (assert (= (sort-with [:breakfast :lunch :dinner]     #{:lunch :breakfast :dinner})
             '(:breakfast :lunch :dinner)))
  (assert (= (sort-with [2 3 4 :jack :queen :king :ace] [4 2 4 :king 2])
             [2 2 4 4 :king]))
  (assert (= (sort-with (range 1000 -1 -2) (range 0 100 2))
             (range 98 -1 -2)))
  (assert (= (sort-with (range 1000) (range 100))
             (range 100)))
  (assert (= (sort-with [] ()) ()))
  true)


(defn df-sort-with [oxs ys]
  (let [keyfn (zipmap oxs (range))]
    (sort-by keyfn ys)))

(defn bn-sort-with [order xs]
  (let [order-map (into {} (map-indexed (fn [idx v] [v idx]) order))]
    (sort-by order-map xs)))
