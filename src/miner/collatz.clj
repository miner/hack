(ns miner.collatz)

;; a couple of fns borrowed from halfbaked.clj

(defn fixed-point
  ([f] (fixed-point f 0))
  ([f guess] (fixed-point f guess 1000))
  ([f guess limit] (fixed-point f guess limit =))
  ([f guess limit eq?]
   (when (pos? limit)
     (let [guess' (f guess)]
       (if (eq? guess' guess)
         guess
         (recur f guess' (dec limit) eq?))))))

(defn converge-seq
  "Eager iteration of f, starting with x.  Terminates when result is repeated
  consecutively (reaches a fixed point).  Equality check calls eq? (default =).  Returns nil
  if limit count is exceeded (default 1000)."
  ([f x] (converge-seq f x 1000 =))
  ([f x limit] (converge-seq f x limit =))
  ([f x limit eq?]
   (if (and limit (pos? limit))
     (let [conv (fn [^long limit res x]
                  (if (eq? (peek res) x)
                    res
                    (when (pos? limit)
                      (recur (dec limit) (conj res x) (f x)))))]
       (conv limit [] x))
     (let [conv (fn [res x]
                  (if (eq? (peek res) x)
                    res
                    (recur (conj res x) (f x))))]
       (conv [] x)))))
   

;; ------------

;; https://en.wikipedia.org/wiki/Collatz_conjecture

;; https://oeis.org/A008908
;; number of steps to get to 1



;; traditional definition
(defn collatz1 [n]
  (if (even? n)
    (quot n 2)
    (inc (* 3 n))))

(defn ncz [n]
  (count (converge-seq collatz1 n 1000 (fn [a _] (= a 1)))))

;; variant for convenient convergence to 1 (terminates instead of oscillating)
(defn cz [n]
  (cond (<= n 1) 1
        (even? n) (quot n 2)
        :else (inc (* 3 n))))


(defn cz1 [n]
  (count (converge-seq cz n)))


;; (map cz1 (range 1 72))
(def ncoll71 [1, 2, 8, 3, 6, 9, 17, 4, 20, 7, 15, 10, 10, 18, 18, 5, 13, 21, 21, 8, 8, 16, 16,
              11, 24, 11, 112, 19, 19, 19, 107, 6, 27, 14, 14, 22, 22, 22, 35, 9, 110, 9, 30,
              17, 17, 17, 105, 12, 25, 25, 25, 12, 12, 113, 113, 20, 33, 20, 33, 20, 20, 108,
              108, 7, 28, 28, 28, 15, 15, 15, 103])







;; WIP -- trying to be normal Clojure
;; need a reduce/iterate-ish thing that runs to zero internal state and yields a result
;; like (converge step pred init)

(defn converge2 [pred2 stepfn init]
  (loop [state init step 0]
    (if (pred2 state step)
      state
      (recur (stepfn state step) (inc step)))))


(defn converge3 [pred3 stepfn init]
  (loop [old nil state init step 0]
    (if (pred3 old state step)
      [state step]
      (recur state (stepfn state (inc step)) (inc step)))))

(defn fn1 [f]
  (fn ([x] (f x)) ([x & _] (f x))))


(defn fn2 [f]
  (fn ([_ x] (f x)) ([_ x & _] (f x))))


(defn converge [pred2 stepfn init]
  (loop [state init step 0]
    (if (pred2 state step)
      [state step]
      (recur (stepfn state step) (inc step)))))





;;;----------------------------------------------------------------------


;;; Variation for Eric's Clojure Challenge
;;;
;;; https://gist.github.com/ericnormand/c2dbd86c1579d7a0c400961110537e72


;; A Collatz sequence for a positive integer n is defined by repeatedly applying the
;; following rules:
;;
;; If n is even, divide by 2.
;; If n is odd, multiply by 3 and add 1.
;; The sequence ends when n = 1.
;; Write a function that generates a lazy Collatz sequence given a number.

;; eager, vector result
(defn eager-collatz [n]
  (loop [cv [n]]
    (let [c (peek cv)]
      (cond (<= c 1) cv
            (even? c) (recur (conj cv (quot c 2)))
            :else (recur (conj cv (inc (* 3 c))))))))

;; lazy version
(defn collatz [n]
  (cond (<= n 1) (list n)
        (even? n) (lazy-seq (cons n (collatz (quot n 2))))
        :else (lazy-seq (cons n (collatz (inc (* 3 n)))))))

;; a bit faster (for small n)
(defn collatz3 [n]
  (if (<= n 1)
    (list n)
    (lazy-seq (cons n (collatz (if (even? n) (quot n 2) (inc (* 3 n))))))))



(defmacro assert?
  ([pred form result]
   `(do (assert (~pred ~form ~result)) true))
  ([pred form result & more]
   `(and (assert? ~pred ~form ~result)
         (assert? ~pred ~@more))))


(defn smoke-collatz [collatz]
  (assert? = (collatz 1)  '(1)
           (collatz 2)  '(2 1)
           (collatz 3)  '(3 10 5 16 8 4 2 1)
           (take 5 (collatz 871)) '(871 2614 1307 3922 1961)))


;; some large results, good for testing laziness
#_
(assert? =
         (count (collatz 871)) 179
         (count (collatz 6171)) 262
         (count (collatz 77031)) 351)

;; @steffan-westcott
(defn sw-collatz [n]
  (take-while some? (iterate #(when (not= % 1)
                                (if (odd? %)
                                  (inc (* 3 %))
                                  (quot % 2)))
                             n)))             
                         

(defn scollatz [n]
  (let [step (fn [c]
               (when (> c 1)
                 (if (even? c)
                   (quot c 2)
                   (inc (* 3 c)))))]
    (sequence (take-while some?) (iterate step n))))

;; somewhat faster than scollatz, but not as good as my collatz
(defn scollatz2 [n]
  (let [step (fn [c]
               (when (> c 1)
                 (if (even? c)
                   (quot c 2)
                   (inc (* 3 c)))))]
    (take-while some? (iterate step n))))
