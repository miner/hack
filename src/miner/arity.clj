(ns miner.arity
  (:require [clojure.tools.macro :as m]))

(defmacro defna [name & dbody]
  ;(println &form)
  `(m/symbol-macrolet [~'&fn ~name]
      (defn ~name ~@dbody)))



(defna foo1
  ([] (&fn 10))
  ([a] {:pre [(or (println &fn) true)]} (&fn a 20))
  ([a b] (&fn a b 30))
  ([a b c] {:post [(== 32 (count %))]}
     (list a b c)))


(defn foo
  ([] (foo 10))
  ([a] (foo a 20))
  ([a b] (foo a b 30))
  ([a b c]  {:post [(== 32 (count %))]}
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


