(ns miner.qs)

;;; original
;;; https://gist.github.com/sergiiriabokon/0c1d5a243d5a197f337fcfde94a2738a

(def col [3 43 5 8 17 33 20])

(defn qs [col]
  (let [a (first col)
        left (filter #(> a %) col)
        right (filter #(< a %) col)]
    (lazy-cat
     (if (empty? left) () (qs left))
     (list a)
     (if (empty? right) () (qs right)))))

;;; SEM: doesn't look like quicksort to me!  He's scanning too much.  I commented on his
;;; gist to warn against time lazy results.  You need to force the computation.

;;; BUG:
#_ (qs [1 1])


;;; concat is already lazy so lazy-cat doesn't help
;;; Had to change the right test to preserve equal values!  Also (rest).  Still not a good idea!
(defn qs1 [col]
  (when-let [a (first col)]
    (let [left (filter #(< % a) (rest col))
          right (filter #(>= % a) (rest col))]
      (concat
       (if (empty? left) () (qs1 left))
       (list a)
       (if (empty? right) () (qs1 right))))))


;;; SEM: I wonder about the recursion.  Could we change it to use recur?

;;;  Well, I actually did this years ago... See quicksort.clj (in hack).  Much better than this,
;;;  but still not good enough.
     

;; longer test data
(def xcol (interleave (range 100) (range 100 0 -1)))

(let [res-col (sort col)
      res-xcol (sort xcol)]
  (defn test-qs [qs]
    (assert (= (qs col) res-col))
    (assert (= (qs xcol) res-xcol))
    true))

