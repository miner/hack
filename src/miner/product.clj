;; Lambda Days 2018 - Mary Sheeran - In Praise of Higher Order Functions
;; https://youtu.be/OKUD56nkqig


(ns miner.product
  (:require [clojure.math.combinatorics :as combo]))


(def xxx [(range 4) (range 3) (range 2) (range 4) (range 5)])

(defn cartesian
  ([] [])
  ([a] [a])
  ([a b] (for [i a j b] [i j]))
  ([a b c] (for [i a j b k c] [i j k]))
  ([a b c & more] (reduce (fn [cs xs] (for [c cs x xs] (conj c x)))
                          (cartesian a b c)
                          more)))


(defn csum [xs]
  (reduce (fn [sum v] (+ sum (reduce + 0 v))) 0 xs))

(defn cartesian1
  ([] [])
  ([a] [a])
  ([a b] (for [i a j b] [i j]))
  ([a b c] (for [i a j b k c] [i j k]))
  ([a b c & more] (reduce (fn [cs xs] (mapcat (fn [x] (map #(conj % x) cs)) xs))
                          (cartesian1 a b c)
                          more)))

;; SEM: look at math.combinatorics for the official solution (lazy)
(def cartpro combo/cartesian-product)


;;
;;   (let [carts 
;;                        step (fn [cs xs] (mapcat (fn [x] (map #(conj % x) cs)) xs))]
;;                    (reduce step carts more))))

                            

(defn prod [xss]
  (apply cartesian xss))





;; Translated to Clojure by SEM.
;; Via Sheeran's Haskell version.
;; Originally from Barron and Strachey. 1966. "Programming"
;; Note: some args are reordered to match Clojure convention.  Clojure reduce is basically
;; foldl.  Clojure doesn't have foldr (not tail recursive).  "Anonymous" fns are named to
;; correspond with Sheeran talk.

;; eager and fast
(defn product [xss]
  (reduce (fn f [yss xs]
            (reduce (fn g [zss x]
                      (reduce (fn h [qss ys]
                                (conj qss (conj ys x)))
                              zss
                              yss))
                    []
                    xs))
          [[]]
          xss))

;; Mysteriously, this version is faster.  Assume input is a vector.  Originally, this was my
;; experiment to get conventional order (per combo/cartesian-product).  I expected extra
;; cost to rseq.  Why is it faster?  Because conjing onto a list is faster than a vector.
(defn product1 [xss]
  (reduce (fn f [yss xs]
            (reduce (fn g [zss x]
                      (reduce (fn h [qss ys]
                                (conj qss (conj ys x)))
                              zss
                              yss))
                    []
                    xs))
          [()]
          (rseq xss)))

;; not much penalty to do the rseq/rev dance
(defn product2 [xss]
  (reduce (fn f [yss xs]
            (reduce (fn g [zss x]
                      (reduce (fn h [qss ys]
                                (conj qss (conj ys x)))
                              zss
                              yss))
                    []
                    xs))
          [()]
          (if (vector? xss) (rseq xss) (reverse xss))))

(defn set= [a b]
  (= (set a) (set b)))


;; Olivier Danvy and Michael Spivey [2007]
;; "On Barron and Strachey’s Cartesian Product Function"
;;
;; product xss = foldr f' u xss id []
;;  where
;;    u h zss = h [] : zss
;;    f' xs c' h zss = foldr g zss xs
;;     where  g x qss = c' (h * (x:)) qss.

;; SEM -- write a Clojure version of that!
(def c-) ;; UNIMPLEMENTED

(defn c [h zss]
  (concat (c- h) zss))

;; (defn prod [xss]
;;   (let [g (fn [qss x]
;;             (c (comp h #(conj % x)) qss))
;;         f (fn [xs c h zss]
;;             (reduce g xs zss))]
;;   (reduce f u xss id [])
;; 
