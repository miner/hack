(ns miner.farg)

;; Experminented with a few implementations of this very simple function.  Perhaps overkill.

;; numbering scheme follows anonymous function notation

(defn farg
  "Returns a function that simply returns its Nth arg.  The first arg is position 1, which
  is the default.  If there is no corresponding arg, the default-value is returned, which
  defaults to nil."
  ([] (farg 1 nil))
  ([n] (farg n nil))
  ([^long n default-value]
   (case n
     1 (fn
         ([] default-value)
         ([a] a)
         ([a b] a)
         ([a b c] a)
         ([a b c & more] a))
     2 (fn
         ([] default-value)
         ([a] default-value)
         ([a b] b)
         ([a b c] b)
         ([a b c & more] b))
     3 (fn
         ([] default-value)
         ([a] default-value)
         ([a b] default-value)
         ([a b c] c)
         ([a b c & more] c))
     (fn
       ([] default-value)
       ([a] default-value)
       ([a b] default-value)
       ([a b c] default-value)
       ([a b c & args] (nth args (- n 4) default-value))))))

