(ns miner.ackermann)

;; Classic definition
;; http://rosettacode.org/wiki/Ackermann_function#Clojure

(defn ackermann [m n] 
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


;; Much faster!
;; derived from CL version, better algorithm but not obviously equivalent
;; and subject to overflow for m > 3
(defn ack2a [m n]
  (case m
    0 (inc n)
    1 (+ n 2)
    2 (+ n n 3)
    3 (- (bit-shift-left 1 (+ 3 n)) 3)
    (recur (dec m) (if (zero? n) 1 (ack2a m (dec n))))))

;; (ack2a 4 2) ===> -2; wrong! because of bit-shift overflow


(defn ack2b ^long [^long m ^long n]
  (case m
    0 (inc n)
    1 (+ n 2)
    2 (+ n n 3)
    3 (- (bit-shift-left 1 (+ 3 n)) 3)
    (recur (dec m) (if (= 0 n) 1 (ack2b m (dec n))))))



;; (ack2a 3 50)
;; 9007199254740989

;; https://github.com/bluemont/ackermann
;; works but very slow compared to ack2a
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
    (when (not= (ackermann m n) (afn m n))
      (println "Failed" m n)))
  (afn 3 4))


;; http://rosettacode.org/wiki/Evaluate_binomial_coefficients#Clojure
;; binomial coefficient

(defn binomial-coefficient [n k]
  (let [rprod (fn [a b] (reduce * (range a (inc b))))]
    (/ (rprod (- n k -1) n) (rprod 1 k))))

(defn coeffs [k]
  (map (partial binomial-coefficients k) (range (inc k))))

(defn bico [n k]
  (let [rprod (fn [a b] (reduce * 1 (range a (inc b))))]
    (/ (rprod (inc (- n k)) n) (rprod 1 k))))

(defn bicos [k]
  (map (partial bico k) (range (inc k))))
