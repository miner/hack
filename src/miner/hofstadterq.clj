(ns miner.hofstadterq)

;; from  _Godel, Esher, Bach_ by Douglas Hofstadter

;; Hofstadter Q sequence
;; https://rosettacode.org/wiki/Hofstadter_Q_sequence

;; It is defined like the Fibonacci sequence, but whereas the next term in the Fibonacci
;; sequence is the sum of the previous two terms, in the Q sequence the previous two terms
;; tell you how far to go back in the Q sequence to find the two numbers to sum to make the
;; next term of the sequence.

;; https://oeis.org/A005185

;; Lisp definition of single item Q
#_
(defun q (n)
  (if (<= n 2) 1
    (+
     (q (- n (q (- n 1))))
     (q (- n (q (- n 2)))))))

;; Usually considered as a sequence


;; Rosetta Clojure solution
(defn qs [q]
  (let [n (count q)]
    (condp = n
      0 [1]
      1 [1 1]
      (conj q (+ (q (- n (q (- n 1))))
                 (q (- n (q (- n 2)))))))))
 
(defn qfirst [n] (-> (iterate qs []) (nth n)))




;; first 10 in sequence:  [1 1 2 3 3 4 5 5 6 6]
;; (q 1000) = 502

;; Extra credit: Count and display how many times a member of the sequence is less than its
;; preceding term for terms up to and including the 100,000th term.
;; 49798


(defn test-qgen-rosetta
  ([] (test-qgen-rosetta qfirst))
  ([qgen]
  (assert (= (qgen 10) [1 1 2 3 3 4 5 5 6 6]))
  (assert (= (last (qgen 1000)) 502))
  (assert (= (->> (qgen 100000) (partition 2 1) (filter #(apply > %)) count)
             49798))
   true))




;; my much faster test
(defn test-qgen
  ([] (test-qgen qfirst))
  ([qgen]
   (assert (= (qgen 10) [1 1 2 3 3 4 5 5 6 6]))
   (assert (= (peek (qgen 1000)) 502))
   (assert (= (first (reduce (fn [[cnt prev] x]
                               (if (< x prev)
                                 [(inc cnt) x]
                                 [cnt x]))
                             [0 0]
                             (qgen 100000)))
              49798))
   true))



(defn qn [n]
  (let [qstep (fn [qv]
                (let [cnt (count qv)]
                  (conj qv (+ (qv (- cnt (qv (dec cnt))))
                              (qv (- cnt (qv (- cnt 2))))))))]
    (case (long n)
      0 []
      1 [1]
      2 [1 1]
      (loop [qv [1 1]]
        (if (< (count qv) n)
          (recur (qstep qv))
          qv)))))


;; if you just want the iTH value
(defn qi [i]
  (peek (qn (inc i))))



;; ---------------------


;; slow recursive definition, uncached
(defn qq [n]
  (if (< n 2)
    1
    (+ (qq (- n (qq (dec n))))
       (qq (- n (qq (- n 2)))))))


;; borrowed from mrfn
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

;; caching improves performance, but still much slower than qn
;; in this case, the previous values are known to be previous in the sequence, so storing in
;; a vector as you compute (see qn) is faster than using a random access map
(defn qm [n]
  (let [mq (mrfn mq [n]
                 (if (< n 2)
                   1
                   (+ (mq (- n (mq (dec n))))
                      (mq (- n (mq (- n 2)))))))]
    (mapv mq (range n))))



;; pop has a slight cost
(defn qn4 [n]
  (let [qstep (fn [qv]
                (let [cnt (count qv)]
                  (conj qv (+ (qv (- cnt (peek qv)))
                              (qv (- cnt (peek (pop qv))))))))]
    (case (long n)
      0 []
      1 [1]
      2 [1 1]
      (loop [qv [1 1]]
        (if (< (count qv) n)
          (recur (qstep qv))
          qv)))))



(defn qn3 [n]
  (let [qstep (fn [qv]
                (let [cnt (count qv)]
                  (conj qv (+ (qv (- cnt (peek qv)))
                              (qv (- cnt (qv (- cnt 2))))))))]
    (case (long n)
      0 []
      1 [1]
      2 [1 1]
      (loop [qv [1 1]]
        (if (< (count qv) n)
          (recur (qstep qv))
          qv)))))






;; not  faster to cache count
(defn qn2 [n]
  (let [qstep (fn [qv]
                (let [cnt (count qv)]
                  (conj qv (+ (qv (- cnt (peek qv)))
                              (qv (- cnt (peek (pop qv))))))))]
    (case (long n)
      0 []
      1 [1]
      2 [1 1]
      (loop [need (- n 2) qv [1 1]]
        (if (pos? need)
          (recur (dec need) (qstep qv))
          qv)))))


;; ----------------------------------------------------------------------
;; Hofstadter-Conway $10K sequence
;; https://oeis.org/A004001
;; interesting discussion about the prize
;;
;; a(n) = a(a(n-1)) + a(n-a(n-1)) with a(1) = a(2) = 1
;; NOTE: 1-based indexing
;;
;; I like to start at 0 index so I will define it accordingly.  That means some other code
;; might be a bit different.

;; 1-based
;; very slow, literal definition
(defn hc1 [n]
  (if (<= n 2)
    1
    (+ (hc (hc (dec n)))
       (hc (- n (hc (dec n)))))))

;; reminds me a bit of the JMC-91 function in nested recursion



(def hc75
  [1, 1, 2, 2, 3, 4, 4, 4, 5, 6, 7, 7, 8, 8, 8, 8, 9, 10, 11, 12, 12, 13, 14, 14, 15, 15, 15,
   16, 16, 16, 16, 16, 17, 18, 19, 20, 21, 21, 22, 23, 24, 24, 25, 26, 26, 27, 27, 27, 28, 29,
   29, 30, 30, 30, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 33, 34, 35, 36, 37, 38, 38, 39, 40,
   41, 42])



;; fixed for zero based offest vs original 1-based: (dec lst)
(defn hcn 
  "Returns sequence of first N Hofstadter-Conway $10K numbers."
  [n]
  (let [hstep (fn [hcv]
                (let [cnt (count hcv)
                      lst (peek hcv)]
                  (conj hcv (+ (hcv (dec lst))
                               (hcv (- cnt lst))))))]
    (case (long n)
      0 []
      1 [1]
      2 [1 1]
      (loop [hcv [1 1]]
        (if (< (count hcv) n)
          (recur (hstep hcv))
          hcv)))))
