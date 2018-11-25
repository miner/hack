(ns miner.pal
  (:require [criterium.core :as crit]
            [clojure.string :as str]))
            

;; An exercise from Apropos Clojure #18 video cast:
;; https://www.youtube.com/watch?v=elF9BPa0Np4
;;
;; Their solution is something like this...

(defn palindrome? [s]
  (= (seq s) (reverse s)))

(defn substrings [s]
  (let [mx (inc (count s))]
    (for [start (range mx)
          end (range (inc start) mx)]
      (subs s start end))))

(defn longest-palindrome [s]
  "Return the longest substring of s that is a palindrome"
  (apply max-key count (filter palindrome? (substrings s))))


;; First improvement: build the strings in length order by slightly rearranging the `for`
;; comprehension.

(defn substrings1 [s]
  (let [cnt (count s)]
    (for [len (range cnt 0 -1)
          start (range (inc (- cnt len)))
          :let [end (+ start len)]]
      (subs s start end))))


;; The first palindrome found is known to be the longest as the candidates are generated in
;; length order, longest first.  The `filter` and `substrings1` are lazy so the process
;; terminates as soon the first palindromic substring is generated.  The smaller substrings
;; are not realized.

(defn longest-palindrome1 [s]
  "Return the longest substring of s that is a palindrome"
  (first (filter palindrome? (substrings1 s))))


;; Second improvement: Use a faster test.  It's not obvious but `clojure.string/reverse` is
;; pretty fast (due to Java interop) and beats using Clojure sequences of characters.

(defn palindrome2? [s]
  (= s (str/reverse s)))

(defn longest-palindrome2 [s]
  (first (filter palindrome2? (substrings1 s))))


;; The fastest implementation is to use Java interop to access characters within the
;; original string without creating new strings.  Only the longest palindromic substring
;; needs to be realized.  The ^String type annotations help the Clojure compiler pick the
;; correct Java methods.

(defn substr-pal? [^String s start end]
  (loop [front start back (dec end)]
    (or (>= front back)
        (and (= (.charAt s front) (.charAt s back))
             (recur (inc front) (dec back))))))

;; fastest  
(defn longest-palindrome3 [^String s]
  (let [cnt (.length s)]
    (first (for [len (range cnt 0 -1)
                 start (range (inc (- cnt len)))
                 :let [end (+ start len)]
                 :when (substr-pal? s start end)]
             (subs s start end)))))



(defn smoke-test [palfn]
  (let [aman "amanaplanacanalpanama"
        junkman (str "aabbccddeeef" aman "xyz")]
    (assert (= "a" (palfn "a")))
    (assert (= "aba" (palfn "abax")))
    (assert (= aman (palfn junkman)))
    aman))

(defn demunge [fnx]
  (let [s (str fnx)
        at (str/last-index-of s "@")
        fname (if at (subs s 0 at) s)]
    (clojure.main/demunge fname)))


(defn run-benchmarks []
  (doseq [palfn [longest-palindrome longest-palindrome1 longest-palindrome2
                 longest-palindrome3]]
    (println "---------------------------") 
    (println "Testing" (demunge palfn))
    (println)
    (crit/quick-bench (smoke-test palfn)))
  (println "---------------------------"))

