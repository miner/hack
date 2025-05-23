(ns miner.ackermann)

;; Classic definition
;; http://rosettacode.org/wiki/Ackermann_function#Clojure

(defn ackermann [m n]
  {:pre [(not (neg? m)) (not (neg? n))]}
  (cond (zero? m) (inc n)
        (zero? n) (ackermann (dec m) 1)
        :else (ackermann (dec m) (ackermann m (dec n)))))

;; (ackermann 3 10) works, but (ackerman 3 11) StackOverflowError

;; But it's faster with recur
(defn ack [m n]
  (cond (zero? m) (inc n)
        (zero? n) (recur (dec m) 1)
        :else (recur (dec m) (ack m (dec n)))))

;; (ack 3 11) works but (ack 3 12) StackOverflowError


;; trampoline-style, but pretty slow (slower than ack), and still overflows
(defn ackt [m n]
  (cond (fn? n) (recur m (trampoline n))
        (zero? m) (inc n)
        (zero? n) (recur (dec m) 1)
        :else (recur (dec m) #(ackt m (dec n)))))


;; bad
;; (defn ack4
;;   ([m n] (ack4 0 m m n))
;;   ([c m2 m n]
;;    (if (zero? c)
;;      (cond (zero? m) (inc n)
;;            (zero? n) (recur 0 m (dec m) 1)
;;            :else (recur 0 m (dec m) (ack4 0 m m (dec n))))
;;      (recur (dec c) (dec m) (dec m) n
;; 




;; Much faster!
;; derived from CL version, better algorithm but not obviously equivalent
;; and subject to overflow for m > 3


;;; make `case` happy with (long x) cast
(defn ack2c [m n]
  (case (long m)
    0 (inc n)
    1 (+ n 2)
    2 (+ n n 3)
    3 (- (bit-shift-left 1 (+ 3 n)) 3)
    (recur (dec m) (if (zero? n) 1 (ack2c m (dec n))))))



;; warning because case m not sure that m is int (or long)
;; added hints, don't matter much

(defn ack2a ^long [^long m ^long n]
  (case m
    0 (inc n)
    1 (+ n 2)
    2 (+ n n 3)
    3 (- (bit-shift-left 1 (+ 3 n)) 3)
    (recur (dec m) (if (zero? n) 1 (ack2a m (dec n))))))

;; (ack2a 4 2) ===> -2; wrong! because of bit-shift overflow



(defn ack-3-debug
"The Ackermann function using a stack and only tail recursion.
  Thanks to Allan Malloy's post at:
  http://grokbase.com/p/gg/clojure/127rbk4518/reduction-to-tail-recursion"
  [m n]
  (loop [^long m m ^long n n stack ()]
    (when-not (empty? stack)
      (when-let [sss (seq (filter #(> ^long % 2) stack))]
      (println (count stack) sss)))
    (cond
      (zero? m) (if (empty? stack)
                  (inc n)
                  (recur (peek stack) (inc n) (pop stack)))
      (zero? n) (recur (dec m) 1 stack)
      :else (recur m (dec n) (conj stack (dec m))))))



;; (ack2a 3 50)
;; 9007199254740989

;; https://github.com/bluemont/ackermann
;; works but very slow compared to ack2a but can handle m=4
(defn ack-3
"The Ackermann function using a stack and only tail recursion.
  Thanks to Allan Malloy's post at:
  http://grokbase.com/p/gg/clojure/127rbk4518/reduction-to-tail-recursion"
  [m n]
  (loop [m m n n stack ()]
    (cond
      (zero? m) (if (empty? stack)
                  (inc n)
                  (recur (peek stack) (inc n) (pop stack)))
      (zero? n) (recur (dec m) 1 stack)
      :else (recur m (dec n) (conj stack (dec m))))))


(defn test-ack [afn]
  (doseq [m (range 4)
          n (range 5)]
    (assert (= (ackermann m n) (afn m n)) (format "Failed %d %d" m n)))
  (afn 3 4))


;; http://rosettacode.org/wiki/Evaluate_binomial_coefficients#Clojure
;; binomial coefficient

(defn binomial-coefficient [n k]
  (let [rprod (fn [a b] (reduce * (range a (inc b))))]
    (/ (rprod (- n k -1) n) (rprod 1 k))))

(defn coeffs [k]
  (map (partial binomial-coefficient k) (range (inc k))))

(defn bico [n k]
  (let [rprod (fn [a b] (reduce * 1 (range a (inc b))))]
    (/ (rprod (inc (- n k)) n) (rprod 1 k))))

(defn bicos [k]
  (map (partial bico k) (range (inc k))))






;;; Ideas from Dan Friedman video talk at ReClojure 2020
;;; https://www.youtube.com/watch?v=qRI1Ved0SfE&t=24913s


;;; I might have remembered it wrong so this needs testing.  Consult with wiki.

(def f+
  (fn f [m n]
    (if (zero? n)
      m
      (+ 1 (f m (dec n))))))

(def f*
  (fn f [m n]
    (if (zero? n)
      0
      (+ m (f m (dec n))))))

;; exponentiation
(def f**
  (fn f [m n]
    (if (zero? n)
      1
      (* m (f m (dec n))))))

;;; looking for a generalizaion

(defn fack [i]
  (cond (zero? i) f+
        (= i 1) f*
        (= i 2) f**
        :else (fn f [m n]
                (if (or (zero? m) (zero? n))
                  (inc n)
                  ((fack (dec i)) m (f m (dec n)))))))


;;; wikipedia explains Friedman's point about +, *, **, etc operations
  
;;; Wikipedia to the rescue
;;; https://en.wikipedia.org/wiki/Ackermann_function


;;; SEM: slight tweak to iter with local name for a bit of performance

(defn iter [f]
  (fn fi [i]
    (if (zero? i)
      (f 1)
      (f (fi (dec i))))))

;; "curried" ack
(defn cack [m]
  (if (zero? m)
    inc
    (iter (cack (dec m)))))

;;; surprisingly about same speed as ack, but much slower than ack2a
(defn ackc [m n]
  ((cack m) n))


;; calls (f init) then f on the nested result `n` times, returning final result.  Zero-th is
;; just init.
(defn iterated [n f init]
  (if (pos? n)
    (recur (unchecked-dec n) f (f init))
    init))


(defn ackf [m n]
  (let [iter (fn [f] (fn [n] (iterated (inc n) f 1)))
        ;; "curried" ack
        cack (fn [m]
               (if (zero? m)
                 inc
                 (iter (cack (dec m)))))]
    ((cack m) n)))


(defn ackf1 [m n]
  (let [iter (fn [f]
               (fn [n]
                 (reduce (fn [r _] (f r)) 1 (range (inc n)))))

        ;; "curried" ack
        cack (fn [m]
               (if (zero? m)
                 inc
                 (iter (cack (dec m)))))]
    ((cack m) n)))



(defn ackfWORKS [m n]
  (let [iter (fn [f]
               (fn [n]
                 (nth (iterate f 1) (inc n))))
        ;; "curried" ack
        cack (fn [m]
               (if (zero? m)
                 inc
                 (iter (cack (dec m)))))]
    ((cack m) n)))






;;;;;; JUNK
(comment
(defn iter [f]
  (fn [n]
    (if (zero? n)
      (f 1)
      (f ((iter f) (dec n))))))

;; "curried" ack
(defn cack [m]
  (if (zero? m)
    inc
    (iter (cack (dec m)))))

;;; surprisingly about same speed as ack, but much slower than ack2a
(defn ackc [m n]
  ((cack m) n))


(defn acke [m n]
  (let [iter (fn [f]
               (fn [n]
                 (if (zero? n)
                   (f 1)
                   (f ((iter f) (dec n))))))
        ;; "curried" ack
        cack (fn [m]
               (if (zero? m)
                 inc
                 (iter (cack (dec m)))))]
    ((cack m) n)))



(defn ackf [m n]
  (let [iter (fn [f]
               (fn fi [n]
                 (if (zero? n)
                   (f 1)
                   (f (fi (dec n))))))
        ;; "curried" ack
        cack (fn [m]
               (if (zero? m)
                 inc
                 (iter (cack (dec m)))))]
    ((cack m) n)))



(let [iter (fn [f]
             (fn fi [n]
               (if (zero? n)
                 (f 1)
                 (f (fi (dec n))))))
      ;; "curried" ack
      cack (fn [m]
             (if (zero? m)
               inc
               (iter (cack (dec m)))))]
  (defn ackg [m n]
    ((cack m) n)))



)
;;; END JUNK
