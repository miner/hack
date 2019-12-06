;;; Levenshtein distance
;;; Challenge https://purelyfunctional.tv/?p=40244
;;; https://en.wikipedia.org/wiki/Levenshtein_distance

(ns miner.leven)

(defn leven-slow [xs ys]
  (cond (= xs ys) 0
        (empty? xs) (count ys)
        (empty? ys) (count xs)
        :else (min (inc (leven-slow (rest xs) ys))
                   (inc (leven-slow xs (rest ys)))
                   (+ (if (= (first xs) (first ys)) 0 1)
                      (leven-slow (rest xs) (rest ys))))))


;; Memoized recursive function, a mash-up of memoize and fn
(defmacro mrfn
  "Returns an anonymous function like `fn` but recursive calls to the given `name` within
  `body` use a memoized version of the function, potentially improving performance (see
  `memoize`).  Only simple argument symbols are supported, not varargs or destructing or
  multiple arities.  Memoized recursion requires explicit calls to `name` so the `body`
  should not use recur to the top level."
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

(defn levenshtein
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (rest xs) ys))
                                     (inc (lev xs (rest ys)))
                                     (+ (if (= (first xs) (first ys)) 0 1)
                                        (lev (rest xs) (rest ys))))))]
    (if (= a b)
      0
      (mrlev (seq a) (seq b)))))



;; somewhat faster to run in reverse
(defn leven2
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (rest xs) ys))
                                     (inc (lev xs (rest ys)))
                                     (+ (if (= (first xs) (first ys)) 0 1)
                                        (lev (rest xs) (rest ys))))))]
    (if (= a b)
      0
      (mrlev (reverse a) (reverse b)))))

;; not faster than leven2
(defn leven3
  "returns the Levenshtein distance between the strings a and b"
  [a b]
  (let [mrlev (mrfn lev [xs ys]
                    (cond (empty? xs) (count ys)
                          (empty? ys) (count xs)
                          :else (min (inc (lev (pop xs) ys))
                                     (inc (lev xs (pop ys)))
                                     (+ (if (= (peek xs) (peek ys)) 0 1)
                                        (lev (pop xs) (pop ys))))))]
    (if (= a b)
      0
      (mrlev (vec a) (vec b)))))




(defn smoke-test-lev
  ([] (smoke-test-lev levenshtein))
  ([lev]
   (assert (zero? (lev "" "")))
   (assert (= 1 (lev "test" "tent")))
   (assert (= 1 (lev "tent" "test")))
   (assert (= 2 (lev "lawn" "flaw")))
   (assert (= 2 (lev "flaw" "lawn")))
   (assert (= 3 (lev "kitten" "sitting")))
   (assert (= 3 (lev "sitting" "kitten")))
   (assert (zero? (lev "levenshtein" "levenshtein")))
   (assert (= 1 (lev "blevenshtein" "alevenshtein")))
   (assert (= 2 (lev "levenshtein" "levenshtien")))
   true))


#_ (quick-bench (smoke-test-lev leven-slow))
;; Execution time mean : 14.2 sec

#_ (quick-bench (smoke-test-lev levenshtein))
;; Execution time mean : 1.25 ms



;; https://gist.github.com/ericnormand/704f07e8aa041edace319afead3aac73
;; Eric's version

(defn elev
  ([a b]
   (let [f (memoize elev)]
    (f f a b (count a) (count b))))
  ([f a b i j]
   (cond
     (zero? i)
     j

     (zero? j)
     i

     :else
     (min
      (inc (f f a b (dec i) j))
      (inc (f f a b i (dec j)))
      (+ (if (= (.charAt ^String a (dec i))
                (.charAt ^String b (dec j)))
           0
           1)
         (f f a b (dec i) (dec j)))))))


;; refactored by SEM
;; fastest
(defn elev2 [a b]
  (let [f (mrfn f [i j]
                (cond
                 (zero? i)  j
                 (zero? j)  i
                 :else (min
                        (inc (f (dec i) j))
                        (inc (f i (dec j)))
                        (+ (if (= (.charAt ^String a (dec i))
                                  (.charAt ^String b (dec j))) 0 1)
                           (f (dec i) (dec j))))))]
    (f (count a) (count b))))


#_ (quick-bench (smoke-test-lev elev2))
;; Execution time mean : 0.6 ms


