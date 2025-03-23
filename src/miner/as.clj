(ns miner.as)

;;; Finally, decided this is not quite a good idea.  Just write the code or make a better
;;; converter function.

;;; I noticed that I do a lot of checking/converting code
#_ (if (class? c) c (class c))

#_ (if (string? s) s (str s))   ;; really better as just (str s)

#_ (as str string?

;;; just add ? to mk-fn, so class -> class?, int -> int?
;;; not always a safe assumption, no long? for example

;;; another convention if Capital Symbol, like Class -> mk-fn = class, and test-fn = class?

(defmacro as
  ([mk-fn expr]
   (let [tf (symbol (str mk-fn "?"))]
     `(as ~mk-fn ~tf ~expr)))
  ([mk-fn test-fn expr]
   `(let [expr# ~expr
         tf# ~test-fn
         mf# ~mk-fn]
     (if (tf# expr#)
        expr#
        (mf# expr#)))))

