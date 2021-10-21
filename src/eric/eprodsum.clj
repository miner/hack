(ns eric.eprodsum)

;; https://gist.github.com/ericnormand/642261cb154219ec6c1fb01c6cab33f9

;; Product of digits of sum.  Write a function that takes one or more numbers as
;; arguments. It should sum them, then multiply the digits. If the answer is one digit long,
;; it's done. If it's more than one digit, repeat and multiply the digits again.


;; a bit faster
(defn sum-prod
  ([n & nums] (sum-prod (reduce + n nums)))
  ([n]
   (if (< n 10)
     n
     (recur (loop [prod 1 n n]
              (if (zero? n)
                prod
                (recur (long (* prod (rem n 10))) (quot n 10))))))))






(defn sp5 [n & nums]
  (loop [n (reduce + n nums)]
    (if (< n 10)
      n
      (recur (loop [prod 1 n n]
               (if (zero? n)
                 prod
                 (recur (long (* prod (rem n 10))) (quot n 10))))))))




(defn sp1
  ([n]
   (loop [prod 1 n n]
     (if (zero? n)
       (if (< prod 10)
         prod
         (recur 1 prod))
       (recur (long (* prod (rem n 10))) (quot n 10)))))
  ([n & nums] (sp1 (reduce + n nums))))



(defn sp3
  ([n & nums] (sp3 (reduce + n nums)))
  ([n]
   (loop [prod 1 n n]
     (if (zero? n)
       (if (< prod 10)
         prod
         (recur 1 prod))
       (recur (long (* prod (rem n 10))) (quot n 10))))))



(defn dtimes [n]
  (loop [prod 1 n n]
    (if (zero? n)
      prod
      (recur (long (* prod (rem n 10))) (quot n 10)))))

(defn sum-prod0
  ([n]
   (let [n (dtimes n)]
     (if (< n 10) n (recur n))))
  ([n & nums]
   (sum-prod0 (reduce + n nums))))




;; not worth the short-circuit on zero prod, at lease for these test examples
(defn dtimes1 [n]
  (loop [prod 1 n n]
    (if (or (zero? prod) (zero? n))
      prod
      (recur (long (* prod (rem n 10))) (quot n 10)))))


(defn sum-prod1 [& nums]
  (loop [n (dtimes (reduce + 0 nums))]
    (if (< n 10)
      n
      (recur (dtimes n)))))
      

(defn smoke-prod [sum-prod]
  (assert (= (sum-prod 4) 4))
  (assert (= (sum-prod 10) 0))
  ;; since the sum is two digits, then 1 * 0
  (assert (= (sum-prod 11 12)  6))
  (assert (= (sum-prod 12 16 223) 0))
  true)



(defn digits [n]
  (loop [digs () n n]
    (if (zero? n)
      digs
      (recur (conj digs (rem n 10)) (quot n 10)))))



;;; prettier but slower
(defn fsp
  ([n]  (first (filter #(< % 10) (iterate dtimes n))))
  ([n & nums] (fsp (reduce + n nums))))


;; very slow -- chunking?
(defn ssp
  ([n]  (first (sequence (filter #(< % 10)) (iterate dtimes n))))
  ([n & nums] (ssp (reduce + n nums))))



(defn xsp
  ([n]  (transduce (filter #(< % 10))
                   (fn ([_ x] (reduced x))
                     ([r] r))
                   nil
                   (iterate dtimes n)))
  ([n & nums] (xsp (reduce + n nums))))



(defn zsp
  ([n]  (transduce identity
                   (fn ([r _]
                        (if (< r 10)
                          (reduced r)
                          (dtimes r)))
                     ([r] r))
                   n
                   (repeat nil)))
  ([n & nums] (zsp (reduce + n nums))))



(defn rsp
  ([n] (if (< n 10)
         n
         (recur (dtimes n))))
  ([n & nums] (zsp (reduce + n nums))))

