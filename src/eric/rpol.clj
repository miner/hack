(ns eric.rpol
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;; https://gist.github.com/ericnormand/be324723b966bba6fd9dd117d6476031

;; reverse polish eval of string
;; All operations are binary.
;; There are some cases where there aren't enough arguments. You should throw an exception.
;; There are some cases where there are too many arguments. Return the result of the last
;; operation performed.


;; ns-resolve is faster than the eval

(defn rpol [s]
  (transduce (map clojure.edn/read-string)
             (fn ([stack x]
                  (if (symbol? x)
                    (let [op2 (peek stack)
                          stack (pop stack)
                          op1 (peek stack)
                          stack (pop stack)]
                      (conj stack ((ns-resolve *ns* x) op1 op2)))
                    (conj stack x)))
               ([stack] (peek stack)))
             nil
             (clojure.string/split s #" +")))


(defmacro assert=
  ([a b] `(assert (= ~a ~b)))
  ([a b & more] `(do (assert= ~a ~b) (assert= ~@more))))

(defn smoke-rpol [rpol]
  (assert=
   (rpol "1") 1
   (rpol "1 2 +") 3
   (rpol "1 2 + 3 +") 6
   (rpol "4 2 * 2 2 + + 8 /") 3/2)
  true)



;; with-in-str, doall is important to defeat laziness.  Otherwise, the binding for *in* is
;; resolved too late after the scope of with-in-str.  But overall this is slower so not
;; worth the trickiness.
(defn rpol3 [s]
    (let [exprs (with-in-str s (doall (take-while some? (repeatedly #(edn/read {:eof nil} *in*)))))]
      (peek (reduce (fn [stack x]
                      (if (symbol? x)
                        (let [op2 (peek stack)
                              stack1 (pop stack)
                              op1 (peek stack1)
                              stack2 (pop stack1)]
                          (conj stack2 (eval (list x op1 op2))))
                        (conj stack x)))
                    []
                    exprs))))


;; a little faster is you hard-wire ops and assume only long numbers
(defn rpol8 [s]
  (peek (reduce (fn [stack x]
                  (let [op (case x
                             "*" *
                             "/" /
                             "+" +
                             "-" -
                             nil)]
                    (if op
                      (conj (pop (pop stack))
                            (op (peek (pop stack)) (peek stack)))
                      (conj stack (Long/parseLong x)))))
                nil
                (clojure.string/split s #" +"))))

