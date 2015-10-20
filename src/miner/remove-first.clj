(ns miner.remove-first)


;; https://gist.github.com/alandipert/5500279
(defn remove-first1 [pred coll]
  (let [[xs ys] (split-with (complement pred) coll)]
    (concat xs (rest ys))))
 

;; https://gist.github.com/alandipert/5500118
(defn remove-first2 [pred coll]
  (apply concat ((juxt take-while (comp rest drop-while)) (complement pred) coll)))


(def remove-first remove-first1)


(remove-first (partial = 3) [1 2 3 1 2 3])
;;; (1 2 1 2 3)
 
(remove-first even? [1 2 3 1 2 3])
;;; (1 3 1 2 3)
 
