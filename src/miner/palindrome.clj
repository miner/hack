(ns miner.palindrome
  (:import java.util.StringBuilder))

;; Bad to use reverse -- linear time!
;; Bad to compare full reverse -- double the effort!
;; But still as fast as any clever algorithm.
(defn npalin? [s]
  (= s (apply str (reverse s))))

;; see also palin? using clojure.string/reverse

;; for reasonably sized strings this is fastest even with the extra compares
;; basically reimplements clojure.string/reverse
(defn spalin? [^String s]
  (= s (-> (StringBuilder. s) .reverse .toString)))

(defn spalin-not-worth-it? [^String s]
  (let [half (quot (.length s) 2)]
  (= (subs s 0 half) (-> (StringBuilder. s) (.reverse) (.substring 0 half)))))

(defn npalin3? [s]
  (let [cs (seq s)]
    (= cs (reverse cs))))

;; Better to use a vector, and get constant time rseq, but still double compares
(defn npalin4? [s]
  (let [vs (vec s)]
    (= vs (rseq vs))))

(defn npalin41? [n]
  (let [s (str n)
        v (vec s)
        h2 (subvec v (quot (inc (count v)) 2))]
    (every? true? (map = v (rseq h2)))))


;; slower
(defn npalin5? [s]
  (let [vs (vec s)
        half (quot (count vs) 2)]
    (= (take half vs) (take half (rseq vs)))))


(defn palin? [s]
  (let [v (vec s)
        len (count v)]
    (loop [front 0 back (dec len)]
      (or (>= front back)
          (and (= (v front) (v back))
               (recur (inc front) (dec back)))))))


(defn rdigits [n]
  (loop [n n ds []]
    (if (< n 10)
      (conj ds n)
      (recur (quot n 10) (conj ds (rem n 10))))))

(defn palvec? [v]
  (loop [front 0 back (dec (count v))]
    (or (>= front back)
        (and (= (v front) (v back))
             (recur (inc front) (dec back))))))

(defn npal? [n]
  (palvec? (rdigits n)))


(defn front-backs [len]
  (let [half (quot len 2)]
    (map vector (range half) (range (dec len) (- len half 1) -1))))

(defn palin3? [s]
  (let [v (vec s)
        len (count v)]
    (every? (fn [[i j]] (= (v i) (v j))) (front-backs len))))



;; ----------------------------------------------------------------------
;; I have not tested or tried to improve these. 
;; Ported from CL so it's not idiomatic Clojure.
;; consider `into` instead of `concat`

;; HN discussion: http://news.ycombinator.com/item?id=1016566
;; http://jng.imagine27.com/articles/2009-12-25-174540_palindromes_clojure.html
(defn longest-pals-fast
  "O(n) time & space complexity"
  [text]

  (letfn [(final-centers
           [n tcenters centers]
           (cond
            (<= n 1)
            centers
            true 
            (let [n (dec n)]
              (recur n
                     (rest tcenters)
                     (concat (vector 
                              (min (first tcenters) n))
                             centers)))))
          
          (ext-centers
           [strn n centers tcenters cdist]
           (cond
            (zero? cdist)
            #(ext-tail strn (inc n) 1 centers)
            (= (dec cdist) (first tcenters))
            #(ext-tail strn n (first tcenters) centers)
            true
            #(ext-centers strn n 
                          (concat 
                           (vector (min (first tcenters) (dec cdist))) 
                           centers)
                          (rest tcenters) (dec cdist))))

          (ext-tail
           [strn n curr-tail centers]
           (cond 
            (> n (dec (count strn)))
            #(final-centers curr-tail centers
                            (concat (vector curr-tail) centers))
            (zero? (- n curr-tail))
            #(ext-centers strn n 
                          (concat (vector curr-tail) centers)
                          centers curr-tail)
            (= (nth strn n) (nth strn (- n curr-tail 1)))
            #(ext-tail strn (inc n) (+ 2 curr-tail) centers)
            true
            #(ext-centers strn n
                          (concat (vector curr-tail) centers)
                          centers curr-tail)))
          
          (pal-around-centers
           [strn]
           (reverse (trampoline #(ext-tail strn 0 0 []))))]

    (pal-around-centers text)))



;; This looks like a slightly updated version using lists instead of vectors.
;; https://code.google.com/p/jngmisc/source/browse/clojure/palindromes/palindromes.clj
(defn longest-pals-fast
  "O(n) time & space complexity"
  [text]

  (letfn [(final-centers
           [n tcenters centers]
           (cond
            (<= n 1)
            centers
            true
            (let [n (dec n)]
              (recur n
                     (rest tcenters)
                     (cons (min (first tcenters) n)
                           centers)))))

          (ext-centers
           [strn n centers tcenters cdist]
           (cond
            (= 0 cdist)
            #(ext-tail strn (inc n) 1 centers)
            (= (dec cdist) (first tcenters))
            #(ext-tail strn n (first tcenters) centers)
            true
            #(ext-centers strn n
                          (cons (min (first tcenters) (dec cdist))
                                centers)
                          (rest tcenters) (dec cdist))))

          (ext-tail
           [strn n curr-tail centers]
           (cond
            (> n (dec (count strn)))
            #(final-centers curr-tail centers
                            (cons curr-tail centers))
            (= (- n curr-tail) 0)
            #(ext-centers strn n
                          (cons curr-tail centers)
                          centers curr-tail)
            (= (nth strn n) (nth strn (- n curr-tail 1)))
            #(ext-tail strn (inc n) (+ 2 curr-tail) centers)
            true
            #(ext-centers strn n
                          (cons curr-tail centers)
                          centers curr-tail)))

          (pal-around-centers
           [strn]
           (reverse (trampoline #(ext-tail strn 0 0 ()))))]

    (pal-around-centers text)))

;; ----------------------------------------------------------------------


;; http://projecteuler.net/index.php?section=problems&id=4

;; A palindromic number reads the same both ways. The largest palindrome made from the product of
;; two 2-digit numbers is 9009 = 91 99.  Find the largest palindrome made from the product of two 3-digit numbers.

(require 'clojure.string)

(defn palin? [s]
  (= s (clojure.string/reverse s)))

(defn num-palin? [num]
  (palin? (str num)))

(defn euler4 []
  (reduce max (filter num-palin? (for [x (range 999 1 -1) y (range (dec x) 1 -1)] (* x y)))))

(defn find-palin-factors [max-factor]
  (filter (fn [[n _]] (num-palin? n)) (for [x (range max-factor 1 -1) y (range (dec x) 1 -1)] [ (* x y) [x y] ] )))

(defn euler4-factors []
  (reduce (fn [cur v] (if (> (v 0) (cur 0)) v cur)) (find-palin-factors 999)))

          
;; http://blog.fogus.me/2015/04/13/palindromic-sequences-in-clojure/
;; Fogus on searching for palindromes

;; basic definition, works for collections and strings
(defn palindrome? [s]
  (= (seq s) (reverse s)))

;; not worth it for reasonably sized input
(defn rpalin? [s]
  (let [cs (seq s)
        cv (vec cs)]
    (= cs (rseq cv))))

