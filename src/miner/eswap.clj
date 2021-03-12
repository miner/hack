(ns miner.eswap
  (:require [clojure.string :as str]))

;;; https://gist.github.com/ericnormand/d744381a3a625af105245fe5f9ecc942
;;;
;;; Write a function that swaps every occurrence of one string with another, and vice
;;; versa. It is like clojure.string.replace but works both ways.

(defn swap [s a b]
  (let [x (str (char 1))]
    (-> s
        (str/replace a x)
        (str/replace b a)
        (str/replace x b))))



;; faster with StringBuilder
(defn swapb [s ^String a ^String b]
  (let [alen (.length a)
        blen (.length b)]
    (loop [s s res (StringBuilder.)]
      (cond (= s "") (.toString res)
            (str/starts-with? s a) (recur (subs s alen) (.append res b))
            (str/starts-with? s b) (recur (subs s blen) (.append res a))
            :else (recur (subs s 1) (.append res (.charAt ^String s 0)))))))



(defn smoke-swap
  ([] (smoke-swap swap))
  ([swap]
   (assert (= (swap "abc" "a" "b") "bac"))
   (assert (= (swap "book" "k" "t") "boot"))
   (assert (= (swap "Closure" "j" "s") "Clojure"))
   (assert (= (swap "bee" "b" "e") "ebb"))
   (assert (= (swap "abc" "1" "2") "abc"))
   true))
