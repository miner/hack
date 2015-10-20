;; https://groups.google.com/forum/?fromgroups#!msg/clojure-dev/CnmyI1yyLTg/GUwJTqa0DqgJ%5B1-25%5D

;; By Stuart Halloway

;; Copyright and license like Clojure, http://clojure.org
(defn add-handler
  "Compose f, an arbitrary function, with a handler. The composed
   function will call f. If f throws an exception, the handler is
   called, with the exception, the function, and the original
   function arguments as its arguments.

   Typically used with a dynamically bound handler to permit error
   handling strategies at the point of an error, without requiring
   extra parameters throughout an API."
  [f handler]
  (if handler 
    (fn [& args]
      (try
       (apply f args)
       (catch Exception e
         (apply handler e f args))))
    f))


(def ^:dynamic
  *bad-number-handler*
  "error handling contract is nothing more than a fn, dynamically bound"
  nil)

(defn parse-int
  "Contrived example that is forced to use exceptions for flow control."
  [s]
  (Long/parseLong s))

(defn better-parse-int
  "Provide a higher arity version that lets the caller pass in a
   strategy for dealing with trouble."
  ([s]
     (Long/parseLong s))
  ([s handler]
     (try
      (better-parse-int s)
      (catch NumberFormatException e
        (handler s)))))

;; bending parse-int to your will
(binding [*bad-number-handler* (constantly 0)]
  (vec
   (map
    (add-handler parse-int *bad-number-handler*)
    ["2" "4" "duck typing rules"])))

;; using better-parse-int
(vec
 (map
  #(better-parse-int % (constantly 0))
  ["2" "4" "duck typing rules"]))
