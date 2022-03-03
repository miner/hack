(ns eric.needle
  (:require [clojure.string :as str]))

;;; https://gist.github.com/ericnormand/6eda605a72b169d62bde12a740eb5bb9

;; Write a function that takes two strings, a needle and a haystack. The function should
;; return true if the needle is found in the haystack, with a few forgiving features:
;;
;; - If the needle is fully contained in the haystack, it should return true.
;; - If the needle would be fully contained in the haystack, but for one or more missing
;;   letters, it should return true.
;; - Don't match the needle across whitespace or other non alphanumeric characters.
;; - Otherwise, return false.



;;; NB: alphanum is OK -- must allow digits. Which is definition of non-whitespace \w

;; My favorite, not submitted because sw was there first and basically the same.
;; somewhat faster to test the literal first
;; slightly faster to be reluctant (at least for given examples)
(defn found? [needle haystack]
  (or (str/includes? haystack needle)
      (boolean (re-find (re-pattern (str/join "\\w*?" needle)) haystack))))



(defn found5? [needle haystack]
  (or (str/includes? haystack needle)
      (boolean (re-find (re-pattern (str/join "\\w*" needle)) haystack))))

;; slightly faster to be "reluctant", not sure it matters
(defn found4? [needle haystack]
  (or (str/includes? haystack needle)
      (boolean (re-find (re-pattern (str/join "[a-zA-Z0-9]*?" needle)) haystack))))

;; short and sweet
(defn found1? [needle haystack]
  (re-find (re-pattern (str/join "[a-zA-Z0-9]*" needle)) haystack))

(defn re-needle [needle]
  (re-pattern (str/join "[a-zA-Z]*" (seq needle))))



(defn smoke-needle [found?]
  (assert (and 
           (true? (found? "abc" "dddabcfff")) ;=> true (direct match)
           (true? (found? "abc" "xyzannbffooc")) ;=> true (add missing "nn" and "ffoo")
           (true? (found? "abc" "xyza12bffooc")) ;=> true (add missing "12" and "ffoo")
           (not (found? "abc" "xyza1 2bffooc")) ;=> false (can't match "1 2")
           (not (found? "abc" "a bc")) ;=> false (don't match across whitespace)
           (not (found? "xxx" "cxccx")) ;=> false (not enough x's)
           (true? (found? "" "")) ;=> true (trivially so)
           (true? (found? "" "aaa")) ;=> true (also trivial)
           ))
  true)



;; basically same as mine
;; @steffan-westcott
(defn sw-found? [needle haystack]
  (-> (clojure.string/join "\\p{Alnum}*" needle)
      re-pattern
      (re-find haystack)
      boolean))
