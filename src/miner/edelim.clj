(ns miner.edelim)

;;; https://gist.github.com/ericnormand/3be1ba7dee96abedd8f5809bdf89d2e9
;;;
;;; Longest Delimited Substring
;;;
;;; A delimited string is a string that starts with a character, ends with the same
;;; character, and doesn't contain the character anywhere else (besides the beginning and
;;; end). Here are some examples:
;;;
;;; "abbbbbbba" is delimited because it starts and ends with \a.
;;;
;;; "ajjjjaffa" is not delimited because, though it starts and ends with \a, it also
;;; contains \a inside.
;;;
;;; "bkfifoifu" is not delimited because it doesn't end with the same character it starts
;;; with.
;;;
;;; "aa" is delimited.
;;;
;;; "aufodiufa" is delimited.
;;;
;;; Your task is to write a function that returns the longest
;;; delimited substring of a given string.  In the case of ties, return the substring that
;;; appears first.


(defn char-index-of [s ch from]
  (.indexOf ^String s ^String (str ch) ^int from))

(defn char-last-index-of [s ch from]
  (.lastIndexOf ^String s ^String (str ch) ^int from))


(defn delimited? [s]
  (= (char-index-of s 1 (first s)) (dec (count s))) )

(defn delimited [s]
  (map (fn [off] (char-index-of s 

    (reduce (fn [res end] (if (char-index-of s start end)
  


(defn smoke-delim [delimited]
  (assert (= (delimited "ffdsfuiofl") "fuiof"))
  (assert (= (delimited "abbcdefg") "bb"))
  (assert (= (delimited "opoiifdopf") "poiifdop"))
  true)


