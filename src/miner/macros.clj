(ns miner.macros)

;; http://stackoverflow.com/questions/3517083/will-the-clojure-compiler-automatically-evaluate-expressions-of-literals-at-comp

;; Never finished, don't remember
(defmacro preprocess [f & args]
  (let [x# (apply (resolve f) args)]
    `~x#))

;; (def a (preprocess some-time-consuming-function some-literal))

;; (def b (preprocess some-other-time-consuming-function a))


;; Reminder on how to hint a java array using string form ^"[Lmy.element.Type;"

#_
(defn dbiflags ^"[Lorg.lmdbjava.DbiFlags;" [flags]
  (into-array DbiFlags flags))


;;; 08/06/17  13:49 by miner -----------------------------------------------------------------


;; SEM -- BUG -- The function name needs ~'first-char to protect it as a simple name.
;; (Caught by Clj 1.9 spec.)

;; https://coderwall.com/p/qx_52w/type-hints-in-clojure-macros
;; Written by Nikita Prokopov

;; Type hints should be added to symbols. Here's how to do it for function's return type:

(defmacro defthreadlocal [name & body]
 `(def ~(with-meta name {:tag ThreadLocal})
    (proxy [ThreadLocal] []
      (initialValue []
        ~@body))))

;; And here's how to do it for args:

(defmacro declare-first-char []
  (let [s (gensym)]
   `(defn first-char [~(with-meta s {:tag String})]
      (.charAt ~s 0))))


;; See also: http://w01fe.com/blog/2009/07/dynamic-type-hints-in-clojure-macros/
;; This Class cls isn't known until runtime.

(defmacro declare-first-char-dynamic [cls]
	(let [x (gensym)]
	  `(defn first-char [~(with-meta x {:tag cls})] 
         (.charAt ~x 0))))


;; SEMD

;; what I want to work
;; but gets reflection warning
(defmacro decl-fch7 [arg]
  `(defn ~'fch7 [~arg]
     (.charAt ^String ~arg 0)))

;; WORKS
(defmacro decl-fch8 [arg]
  `(defn ~'fch8 [~arg]
     (#(.charAt ^String % 0) ~arg)))


;; Works -- note the ~'fch1 to protect fn name
(defmacro decl-fch1 [cls]
  (let [x 'my-str]
    `(defn ~'fch1 [~(with-meta x {:tag cls})]
       (.charAt ~x 0))))


;; BAD reflection warning
(defmacro decl-fch-bad2 [cls]
  (let [x (vary-meta 'my-str {:tag cls})]
    `(defn ~'fch2 [~x]
       (.charAt ~x 0))))

;; BAD reflection warning
(defmacro bad-decl-fch [arg]
  `(defn ~'fch3 [~arg]
     (let [x# ^String ~arg]
     (.charAt x# 0))))


;; WORKS
(defmacro decl-fch [arg]
  `(defn ~'fch4 [~arg]
     (let [chat# #(.charAt ^String % 0)]
       (chat# ~arg))))

;; WORKS
(defmacro decl-fch5 [arg]
  (let [chat #(.charAt ^String % 0)]
     `(defn ~'fch5 [~arg]
       (~chat  ~arg))))

;; WORKS, might be cleaner to define chat once and have the macro always call it.
;; But maybe the compiler, runtime is smart enough to hoist a constant fn????
(let [chat #(.charAt ^String % 0)]
  (defmacro decl-fch6 [arg]
    `(defn ~'fch6 [~arg]
       (~chat  ~arg))))
