;;; 09/28/15  17:28 by miner -- from the mailing list
;; generalization of iterate
;; F taking as many args as initially given

(defn generate
  [f & more]
  (letfn [(recurse
            [coll args]
            (let [next-val (apply f args)]
              (lazy-cat coll (recurse [next-val] (conj (butlast args) next-val)))))]
    (recurse more (reverse more))))



 (defn generate' [f h & r]
  (cons h
    (lazy-seq
      (apply generate' f
        (reverse (cons (apply f h r) (reverse r)))))))


(defn iteraten
  ([f a] (iterate f a))
  ([f a b] (lazy-seq (cons a (iteraten f b (f a b)))))
  ([f a b c] (lazy-seq (cons a (iteraten f b c (f a b c)))))
  ([f a b c & more]
   (lazy-seq (cons a (apply iteraten f b c (concat more (list (apply f a b c more))))))))

