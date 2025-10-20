(ns miner.arity
  (:require [clojure.tools.macro :as m]))

;;; https://ask.clojure.org/index.php/8765/multi-arity-functions-alias-like-recur


(defn foo
  ([] (foo 10))
  ([a] (foo a 20))
  ([a b] (foo a b 30))
  ([a b c]  {:post [(== 32 (count %))]}
     (list a b c)))

;;; A common mistake is to rename the `foo` at the top but forget to change the internal
;;; self calls.  I want a form of self call that doesn't need to change.  Like `recur` in a
;;; sense but allows different arity references.

;;; The idea is to add a special self-reference symbol to the defn.  In particular, &fn will
;;; act the same as a self call so you can conveniently change the top level function name
;;; without have to change the internal references that are common in mult-arity
;;; definitions.

(defmacro defna [name & dbody]
  ;(println &form)
  `(m/symbol-macrolet [~'&fn ~name]
      (defn ~name ~@dbody)))

(defna foo1
  ([] (&fn 10))
  ([a] {:pre [(or (println &fn) true)]} (&fn a 20))
  ([a b] (&fn a b 30))
  ([a b c] {:post [(== 3 (count %))]}
     (list a b c)))


;;; SEM my version without needing symbol-macrolet
(defmacro defnb [name & dbody]
  `(do (declare ~name)
    (let [~'&fn (var ~name)]
      (defn ~name ~@dbody))))

(defnb foo2
  ([] (&fn 100))
  ([a] {:pre [(or (println &fn) true)]} (&fn a 200))
  ([a b] (&fn a b 300))
  ([a b c] {:post [(== 3 (count %))]}
     (list a b c)))




;; use recur with extra arg to mean self-call  
#_ (defn foobar
  ([] (recur 10))
  ([a] (recur a 20))
  ([a b] (recur a b 30))
  ([a b c] (list a b c)))

(defn bar [x] 
  {:pre [(neg? x)] :post [(pos? %) (even? %)]} 
  (* -3 x))


;;; note that you can have two merged meta attr-maps in multi-arity style.  Doc string is
;;; optional.


(defn quux "quuxize" {:aaa 1}
  ([x]  (list 'quux x))
  ([x y] (quux [x y]))
  {:foo 42})

