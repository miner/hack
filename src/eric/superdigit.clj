(ns eric.superdigit)

;;; https://gist.github.com/ericnormand/049ace0f757e468f7536c7fc181a90a4

;;; Super Digit

;; You're given an integer n and an integer k. There is an integer p that is k instances of
;; the digits of n concatenated together. For example:
;;
;; n=123, k=3 -> p=123123123
;; n=32, k=6 -> p=323232323232
;; n=24543, k=125 -> p=245432454324543245432454324543...
;;
;; Now, take that number p and find its superdigit. The superdigit is defined as follows:
;;
;; superdigit(d) = d                             if # of digits = 1
;; superdigit(d) = superdigit(sum(digits of d))  otherwise
;;
;; That is, if the number has one digit, the superdigit is the number. (Example:
;; superdigit(4)=4). Otherwise, sum the digits and take the superdigit of the
;; result. (Example: superdigit(23)=superdigit(2+3)=5).
;;
;; Your task is to write a function that calculates the superdigit of n and k.  Your answer
;; should always be a single digit.  Be sure that it works with very large strings of
;; digits, such as with n>10^20, k>10^9.




;; The one true assert=.  I have lots of variations but this is the best.
(defmacro assert=
  ([] true)
  ([form result & more]
   `(do (assert (= ~form ~result))
        (assert= ~@more))))

(defn smoke-sd [superdigit]
  (assert= 
   (superdigit 1  1) 1
   (superdigit 10 2) 2
   (superdigit 11 3) 6
   (superdigit 38 7) 5
   (superdigit 38789789374294723947328946 1000000000000000)  8))


(defn superdigit [n k]
  (let [sumdig (fn [n]
                 (loop [sum 0 x n]
                   (if (zero? x)
                     sum
                     (recur (long (+ sum (rem x 10))) (quot x 10)))))]
    (loop [ksum (* k (sumdig n))]
      (if (< ksum 10)
        ksum
        (recur (sumdig ksum))))))


;;; WORKS
(defn superdigit-works [n k]
  (loop [sum 0 x n]
    (if (zero? x)
      (let [ksum (* k sum)]
        (if (< ksum 10)
          ksum
          (superdigit ksum 1)))
      (recur (long (+ sum (rem x 10))) (quot x 10)))))





(defn idigs [n] (map #(rem % 10) (take-while pos? (iterate #(quot % 10) n))))

(defn sumd [n] (reduce (fn [r x] (+ r (rem x 10))) 0 (take-while pos? (iterate #(quot % 10) n))))

(defn tsumd [n]
  (transduce (take-while pos?)
             (fn ([r x] (+ r (rem x 10))) ([r] r))
             0 
             (iterate #(quot % 10) n)))


;; nice but slower
(defn tsd [n k]
  (let [p (transduce (take-while pos?)
                     (fn ([r x] (+ r (rem x 10))) ([r] (* k r)))
                     0 
                     (iterate #(quot % 10) n))]
    (if (< p 10)
      p
      (recur p 1))))

;; and slower, but maybe nicer?
(defn tsd2 [n k]
  (let [p (transduce (comp (take-while pos?)
                           (map #(rem % 10)))
                     (completing + #(* k %))
                     0 
                     (iterate #(quot % 10) n))]
    (if (< p 10)
      p
      (recur p 1))))


;; looks better, less trickey with K
(defn tsd3 [n k]
  (let [sumd (fn [n] (transduce (comp (take-while pos?)
                                      (map #(rem % 10)))
                                +
                                0 
                                (iterate #(quot % 10) n)))]
    (loop [p (* k (sumd n))]
      (if (< p 10)
        p
        (recur (sumd p))))))


(defn tsd4 [n k]
  (let [sumd (fn [n] (transduce (comp (halt-when zero? +)
                                      (map #(rem % 10)))
                                +
                                0 
                                (iterate #(quot % 10) n)))]
    (loop [p (* k (sumd n))]
      (if (< p 10)
        p
        (recur (sumd p))))))




(defn rsd [n k]
  (let [sumd (fn [n] (reduce + 0 (mapv #(- (long %) (long \0)) (str n))))]
    (loop [p (* k (sumd n))]
      (if (< p 10)
        p
        (recur (sumd p))))))


(defn rsd2 [n k]
  (let [sumd (fn [n] (reduce + 0 (mapv #(- (long %) 48) (str n))))]
    (loop [p (* k (sumd n))]
      (if (< p 10)
        p
        (recur (sumd p))))))



;;; fastest
(defn rsd4 [n k]
  (let [sumd (fn [n] (reduce (fn [r d] (+ r (- (long d) (long \0))))
                             0
                             (str n)))]
    (loop [p (* k (sumd n))]
      (if (< p 10)
        p
        (recur (sumd p))))))


;; not faster than rsd4, but fastest of the tranducers
(defn tsd5 [n k]
  (let [sumd (fn [n] (transduce (map #(- (long %) (long \0)))
                                +
                                0
                                (str n)))]
    (loop [p (* k (sumd n))]
      (if (< p 10)
        p
        (recur (sumd p))))))
