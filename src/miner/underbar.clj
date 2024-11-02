(ns miner.underbar
  (:require [clojure.string :as str])
  (:require [clojure.edn :as edn]))

;;; https://ask.clojure.org/index.php/8511/add-digit-separators-support-to-number-literals
;;;
;;; A user asks for allowing underbars in numeric literals as supported by Java and Go.
;;; Some people pointed out that adding that feature would require a lot of work on various
;;; parsing tools and code readers so it's a tough sell.  I responded that anyone can add a
;;; macro to handle underbar literals.

(defmacro und [lit]
  (if-let [s# (if (string? lit) lit (when (instance? clojure.lang.Named lit) (name lit)))]
    `~(clojure.edn/read-string (clojure.string/replace s# "_" ""))
    lit))

;;; valid uses returning 123456
;;; note: bare symbol style must start with _ to make it a legal symbol.
(und 123456)
(und _123_456)
(und :123_456)
(und "123_456")

