(ns miner.hofstadterq)

;; from  _Godel, Esher, Bach_ by Douglas Hofstadter

;; Hofstadter Q sequence
;; https://rosettacode.org/wiki/Hofstadter_Q_sequence

;; It is defined like the Fibonacci sequence, but whereas the next term in the Fibonacci
;; sequence is the sum of the previous two terms, in the Q sequence the previous two terms
;; tell you how far to go back in the Q sequence to find the two numbers to sum to make the
;; next term of the sequence.


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





;; ---------------------


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


