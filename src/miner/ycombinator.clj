(ns miner.ycombinator)

;;; http://blog.klipse.tech/lambda/2016/08/10/y-combinator-app.html
;;; Y-Combinator in Clojure
;;; by Yehonathan Sharvit "viebel"

;;; Related:
;;; https://stackoverflow.com/questions/3906831/how-do-i-generate-memoized-recursive-functions-in-clojure


(def Ywrap
  (fn [wrapper-func f]
    ((fn [x]
       (x x))
     (fn [x]
       (f (wrapper-func (fn [y]
                      ((x x) y))))))))

 (defn memo-wrapper-generator [] 
   (let [hist (atom {})]
    (fn [f]
      (fn [y]
        (if (find @hist y)
          (@hist y)
         (let [res (f y)]
           (swap! hist assoc y res)
        res))))))

(def Ymemo 
  (fn [f]
   (Ywrap (memo-wrapper-generator) f)))

(def factorial-gen
  (fn [func]
    (fn [n]
      ;;(println n)
     (if (zero? n)
      1
      (* n (func (dec n)))))))

(def factorial-memo (Ymemo factorial-gen))




;;; My version of above to have localized memoization
(defn yfact [n]
  (let [yf (Ymemo (fn [func]
                    (fn [n]
                      (if (zero? n)
                        1
                        (* n (func (dec n)))))))]
    (yf n)))




#_ (quick-bench (yfact 20))
;; Execution time mean : 3.408493 µs






;;; My take on something similar.  You don't really need the Y-combinator in Clojure as we
;;; already have recursion.  My mrfn macro let's use program more like a normal function
;;; definition.

;; BUG: what if body uses recur?  The args won't match.  You have to add an extra arg for
;; the `name`.

;; mash-up of memoize and fn
(defmacro mrfn
  "Returns an anonymous function like `fn` but recursive calls to the given `name` within
  `body` use a memoized version of the function, potentially improving performance (see
  `memoize`).  Only simple argument symbols are supported, not varargs or destructing or
  multiple arities."
  [name args & body]
  {:pre [(simple-symbol? name) (vector? args) (seq args) (every? simple-symbol? args)]}
  (let [akey (if (= (count args) 1) (first args) args)]
    ;; name becomes extra arg to support recursive memoized calls
    `(let [f# (fn [~name ~@args] ~@body)
           mem# (atom {})]
       (fn mr# [~@args]
         (if-let [e# (find @mem# ~akey)]
           (val e#)
           (let [ret# (f# mr# ~@args)]
             (swap! mem# assoc ~akey ret#)
             ret#))))))

;; basic
(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (dec n)))))

;; Dynamic memoization, once for top-level call, but not held on to forever.  I think this
;; is the best approach to using memoization.
(defn mfact [n]
  (let [mf (mrfn mfact [n]
                 (if (zero? n)
                   1
                   (* n (mfact (dec n)))))]
    (mf n)))

;; permanent memoization, not what I advocate
(def pfact (mrfn mfact [n]
                 (if (zero? n)
                   1
                   (* n (mfact (dec n))))))


#_ (quick-bench (mfact 20))
;; Execution time mean : 3.331594 µs


;;; https://stackoverflow.com/questions/9898069/in-clojure-is-it-possible-to-combine-memoization-and-tail-call-optimization

;;; answers from cgrand and kotarak

;;; more similar macro stuff, but using delays

;;; fib would be a better example than factorial

;;; other questions about supporting tail recursion.  I thought this would only be used
;;; where tail recursion wasn't possible.  Needs more thought.
