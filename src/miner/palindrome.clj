(ns miner.palindrome
  (:import java.lang.StringBuilder))

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
;;
;; old HN discussion from 2009: http://news.ycombinator.com/item?id=1016566
;; http://jng.imagine27.com/articles/2009-12-25-174540_palindromes_clojure.html
;; apparently moved to
;; http://jng.imagine27.com/index.php/2009-12-25-174540_palindromes_clojure.html
;;
;; The author seems to be a Clojure newbie.  I used to have the code here to see what I
;; could do with it. Not good Clojure code.  Deleted.
;;
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


;; ----------------------------------------------------------------------

;; An exercise from Apropos Clojure #18 video cast:
;; https://www.youtube.com/watch?v=elF9BPa0Np4

;; Their solution is something like this...

(defn substrings [s]
  (let [mx (inc (count s))]
    (for [start (range mx)
          end (range (inc start) mx)]
      (subs s start end))))

(defn longest-palindrome [s]
  "Return the longest substring of s that is a palindrome"
  (apply max-key count (filter palindrome? (substrings s))))



;; SEM but it's slow to build strings and tear them apart
;; do all your work in vectors and indices
;; try longer stuff first

;; first, build the strings in length order
(defn substrings1 [s]
  (let [cnt (count s)]
    (for [len (range cnt 0 -1)
          start (range (inc (- cnt len)))
          :let [end (+ start len)]]
      (subs s start end))))

;; No need to max-key, the first palindrome is the longest by construction now.  Laziness
;; lets the process terminate as soon a a palindrome substring is generated.  The smaller
;; substrings are not realized.

(defn longest-palindrome1 [s]
  "Return the longest substring of s that is a palindrome"
  (first (filter palindrome? (substrings1 s))))




(defn subvec-pal1? [v start end]
  ;; start inclusive, end exclusive
  (let [sv (subvec v start end)]
    (= (seq sv) (rseq sv))))



(defn indices-by-length [cnt]
  (for [len (range cnt -1 -1)
        start (range (- cnt len))]
    [start (+ start len 1)]))

;; SEM -- much faster to just work the indices, with longest candidates first
(defn long-pal1 [s]
  (let [vc (vec s)
        pairs (indices-by-length (count vc))
        [start end] (first (filter (fn [[start end]] (subvec-pal1? vc start end)) pairs))]
    (when start
      (subs s start end))))





(defn subvec-pal? [v start end]
  (loop [front start back (dec end)]
    (or (>= front back)
        (and (= (v front) (v back))
             (recur (inc front) (dec back))))))

(defn long-pal2 [s]
  (let [vc (vec s)
        cnt (count vc)]
    (first (for [len (range cnt 0 -1)
                 start (range (inc (- cnt len)))
                 :let [end (+ start len)]
                 :when (subvec-pal? vc start end)]
             (subs s start end)))))




(when-not (satisfies?   clojure.core.protocols/IKVReduce (subvec [1] 0))
  (extend-type clojure.lang.APersistentVector$SubVector
    clojure.core.protocols/IKVReduce
    (kv-reduce
      [subv f init]
      (let [cnt (.count subv)]
        (loop [k 0 ret init]
          (if (< k cnt)
            (let [val (.nth subv k)
                  ret (f ret k val)]
              (if (reduced? ret)
                @ret
                (recur (inc k) ret)))
            ret))))))



;; slower
(defn subvec-pal2? [v start end]
  (let [sv (subvec v start end)
        cnt (count sv)]
    (reduce-kv (fn [r i c]
                 (cond (>= i r) (reduced true)
                       (not= (sv i) (sv r)) (reduced false)
                       :else (dec r)))
               (dec cnt)
               sv)))

;; not better
(defn subvec-pal3? [v start end]
  (let [sv (subvec v start (quot (+ start end) 2))
        rsv (subvec v (quot (+ start end 1) 2) end)]
    (= (seq sv) (rseq rsv))))

;; not better
(defn subvec-pal4? [v start end]
  (let [sv (subvec v start end)]
    (= (seq sv) (rseq sv))))

(defn long-pal2f [spalfn s]
  (let [vc (vec s)
        cnt (count vc)]
    (first (for [len (range cnt 0 -1)
                 start (range (inc (- cnt len)))
                 :let [end (+ start len)]
                 :when (spalfn vc start end)]
             (subs s start end)))))


;;; Fastest implementation is to use interop to access chars in string.  Don't create new
;;; strings.  Don't need to vectorize.  Just .charAt.  Still important to consider longest
;;; candidates first.

(defn substr-pal? [^String s start end]
  (loop [front start back (dec end)]
    (or (>= front back)
        (and (= (.charAt s front) (.charAt s back))
             (recur (inc front) (dec back))))))

;; fastest  
(defn long-pal [^String s]
  (let [cnt (.length s)]
    (first (for [len (range cnt 0 -1)
                 start (range (inc (- cnt len)))
                 :let [end (+ start len)]
                 :when (substr-pal? s start end)]
             (subs s start end)))))


(def aman "amanaplanacanalpanama")
(def junkman (str "aabbccddeeef" aman))

(defn smoke [palfn]
  (assert (= "a" (palfn "a")))
  (assert (= "aba" (palfn "abax")))
  (assert (= aman (palfn junkman)))
  aman)




;; SEM: probably should start search in middle of string??? and build out like A* or do
;; jumps of your current width like Boyer-Moore.  I guess that's what was supposed to happen
;; with the CL ports, but they're not quite right.

