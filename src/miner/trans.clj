(ns miner.trans)

(defn trans
  "Like `comp` but handles the arguments in reverse order, which might seem more natural for
  composing transducers.  Basically, the last transducer defines the first function that
  will operate on the input."
  ([] identity)
  ([f] f)
  ([f g]
     ;; transducer functions need to implement only arities 0, 1, and 2."
     (fn 
       ([] (g (f)))
       ([x] (g (f x)))
       ([x y] (g (f x y)))))
  ([f g h] 
     (fn 
       ([] (h (g (f))))
       ([x] (h (g (f x))))
       ([x y] (h (g (f x y))))))
  ([f g h & fs]
     (reduce trans (trans f g h) fs)))


     ;; was (apply comp (reduce conj (list h g f) fs))))


;; need to test (trans (trans f g) (trans h i))
;; compared to (trans f g h i)


;;; (reduce trans () (list* f g h fs))

;; (reduce trans (trans f g h) fs)


;; wondering about trans->
;; some way of using -> with transducers

#_ (def trans-> comp)

;; (transduce xform f init coll)
;; (transduce xform f coll)

;; bad idea, too complicated for little benefit, better to use trans directly
#_ (defn trans-> [f init coll & xforms]
     (transduce (apply trans xforms) f init coll))
