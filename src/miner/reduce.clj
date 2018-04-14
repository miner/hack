(ns miner.reduce
  (:require [clojure.reflect :as r]))



;; HOF for using (reduced x) to short-circuit reduce
;; note: result can be a fn or a constant.  If fn, result is (result accumulation item)
;; maybe could have used ifn?, but that would treat sets and maps as functions instead of values
(defn freduced [f pred result]
  (let [rfn (if (fn? result) result (constantly result))]
    (fn [accum item]
      (if (pred item) (reduced (rfn accum item)) (f accum item)))))

;; tough call: 1-based or 0-based?  The #(...) uses % or %1 for first arg so I went with 1-based

;; numbering scheme follows anonymous function notation
(defn farg
  "Returns a function that simply returns its Nth arg.  The first arg is position 1, which
  is the default.  If there is no corresponding arg, the default-value is returned, which
  defaults to nil."
  ([] (farg 1 nil))
  ([n] (farg n nil))
  ([^long n default-value]
   (fn
     ([] default-value)
     ([a] (case n 1 a default-value))
     ([a b] (case n 1 a 2 b default-value))
     ([a b c] (case n 1 a 2 b 3 c default-value))
     ([a b c & args] (case n 1 a 2 b 3 c (nth args (- n 4) default-value))))))


(comment
  (reduce (freduced + nil? nil) [1 2  nil 3 4 5])
  ;; nil
  (reduce (freduced + neg? (farg)) [1 2 3 -100 4 5])
  ;; 6
  )

;; from clojure group, by Ben Wolfson <wolfson@gmail.com>
;; what Haskell calls unfold; basically the inverse of reduce

(defn expand
 [f seed]
 (lazy-seq (when-let [[a b] (f seed)] (cons a (expand f b)))))

;; could be done with iterate as well

(defn frot [f]
  "Returns a function similar to f but the argument positions rotated such that
   the returned function takes the last arg of f as its first.
   Could be useful when using the threading macro ->"
  (fn
    ([] (f))
    ([a] (f a))
    ([a b] (f b a))
    ([a b c] (f c a b))
    ([a b c & args] (apply f (last args) a b c (butlast args)))))


;; http://stackoverflow.com/questions/1696693/clojure-how-to-find-out-the-arity-of-function-at-runtime
;; SEM: but what about multiple arity functions?  Not so good.
(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes ^java.lang.reflect.Method m)]
    (alength p)))



;; https://groups.google.com/forum/?fromgroups=#!topic/clojure/2ZU62kgGjXg
(defn old-arities [f]
   (let [methods      (.getDeclaredMethods (class f))
         count-params (fn [m]
                        (map #(count (.getParameterTypes ^java.lang.reflect.Method %))
                             (filter #(= m (.getName ^java.lang.reflect.Method %)) methods)))
         invokes      (count-params "invoke")
         do-invokes   (map dec (count-params "doInvoke"))
         arities      (sort (distinct (concat invokes do-invokes)))]
     (if (seq do-invokes)
       (concat arities [:more])
       arities)))

(defn fake-args [n & [more]]
  (let [basics '[x y z w u v a b c d e f g]
        args (if (<= n (count basics))
               (subvec basics 0 n)
               (vec (map #(symbol (str "x" %)) (range 1 (inc n)))))]
    (if more
      (vec (concat args '[& more]))
      args)))
      
            
        
;; SEM: this should be better since we use the official reflection API,
;; but there's no guarantee that Clojure will always create Java methods
;; like this.
(defn reflect-arities [f]
   (let [methods      (:members (r/reflect (class f)))
         count-params (fn [m] (map #(count (:parameter-types %))
                                   (filter #(= m (:name %))  
                                           methods)))
         invokes      (count-params 'invoke)
         do-invokes   (map dec (count-params 'doInvoke))
         arities      (sort (distinct (concat invokes do-invokes)))]
     (if (seq do-invokes)
       (concat arities [:more])
       arities)))

(defmacro arities [fname]
  {:pre [(symbol? fname)]}
  `(:arglists (meta (var ~fname))))


;;; 02/20/18  18:19 by miner --
;; https://stackoverflow.com/questions/48874469/clojure-partition-list-of-strings-accumulating-the-result

(defn catsize [limit strs]
  (reduce (fn [res s]
            (let [base (peek res)]
              (if (> (+ (.length ^String base) (.length ^String s)) limit)
                (conj res s)
                (conj (pop res) (str base s)))))
          (if (seq strs) [(first strs)] [])
          (rest strs)))



;; stackoverflow.com

;; for is faster than mine
(defn repeated [coll]
  (for [[k v] (frequencies coll) :when (not= v 1)] k))

;; SEM: I like reduce-kv but it's not as fast as I thought
(defn sem-rep1 [coll]
  (reduce-kv (fn [r k v] (if (= v 1) r (conj r k))) nil (frequencies coll)))

;; slower
(defn sem-rep [coll]
  (reduce-kv (fn [r k v] (if (= v 1) r (conj r k)))
             nil
             (reduce (fn [counts x]
                       (assoc counts x (inc (get counts x 0))))
                      {} coll)))
