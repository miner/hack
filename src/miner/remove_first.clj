(ns miner.remove-first)

;; Alan Dipert was joking about some of his variations (I think), but split-with is surprising fast

;; https://gist.github.com/alandipert/5500279
(defn remove-first [pred coll]
  (let [[xs ys] (split-with (complement pred) coll)]
    (concat xs (rest ys))))
 

#_ (remove-first (partial = 3) [1 2 3 1 2 3])
; (1 2 1 2 3)
 
#_ (remove-first even? [1 2 3 1 2 3])
; (1 3 1 2 3)

;; My test, trying to stay lazy
#_ (time (take 20 (drop 1000 (remove-first #(and (> % 1005) (even? %)) (range 100000)))))
;"Elapsed time: 0.814 msecs"
;(1000 1001 1002 1003 1004 1005 1007 1008 1009 1010 1011 1012 1013 1014 1015 1016 1017 1018 1019 1020) 


;; slower -- over 0.9
(defn remfirst-loop [pred coll]
  (loop [head [] remaining coll]
    (if (pred (first remaining))
      (concat head (rest remaining))
      (recur (conj head (first remaining)) (rest remaining)))))

;; not so good, around 0.83
(defn remfirst-trans [pred coll]
  (loop [head (transient []) remaining coll]
    (if (pred (first remaining))
      (concat (persistent! head) (rest remaining))
      (recur (conj! head (first remaining)) (rest remaining)))))

;; can be very slow because it has to realize the whole collection
(defn remfirst-eager [pred coll]
  (loop [head (transient []) remaining coll]
    (if (pred (first remaining))
      (persistent! (reduce conj! head (rest remaining)))
      (recur (conj! head (first remaining)) (rest remaining)))))

;; fastest -- about 0.62
(defn remfirst [pred coll]
  (let [head (take-while (complement pred) coll)]
    (concat head (drop (inc (count head)) coll))))

