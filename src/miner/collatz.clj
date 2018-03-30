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
(defn collatz [n]
  (if (even? n)
    (/ n 2)
    (inc (* 3 n))))

(defn ncz [n]
  (count (converge-seq collatz n 1000 (fn [a _] (= a 1)))))

;; variant for convenient convergence to 1 (terminates instead of oscillating)
(defn cz [n]
  (cond (<= n 1) 1
        (even? n) (/ n 2)
        :else (inc (* 3 n))))


(defn cz1 [n]
  (count (converge-seq cz n)))



(def ncoll71 [1, 2, 8, 3, 6, 9, 17, 4, 20, 7, 15, 10, 10, 18, 18, 5, 13, 21, 21, 8, 8, 16, 16,
              11, 24, 11, 112, 19, 19, 19, 107, 6, 27, 14, 14, 22, 22, 22, 35, 9, 110, 9, 30,
              17, 17, 17, 105, 12, 25, 25, 25, 12, 12, 113, 113, 20, 33, 20, 33, 20, 20, 108,
              108, 7, 28, 28, 28, 15, 15, 15, 103])



