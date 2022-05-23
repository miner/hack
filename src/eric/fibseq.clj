(ns eric.fibseq)

;; https://gist.github.com/ericnormand/377523537b6f13e34b19cdd56ab48de8

;; We all know that the fibonacci sequence is defined as:

;; (fib 0) ;=> 1
;; (fib 1) ;=> 1
;; (fib n) ;=> (+ (fib (dec n)) (fib (dec (dec n))))
;; 
;; And we know we could generate it forward, one element at a time, in a lazy fashion. That
;; is your first task!
;;
;; (fib-seq) ;=> (1 1 2 3 5 8 13 ....) lazily generated because it's infinite


(defn fib-seq
  ([] (fib-seq + 1 1))
  ([op x y]
   (map #(% 0) (iterate (fn [[a b]] [b (op a b)]) [x y]))))


;; same, so no advantage
(defn sfb
  ([] (fib-seq + 1 1))
  ([op x y]
   (sequence (map #(% 0)) (iterate (fn [[a b]] [b (op a b)]) [x y]))))


;;; fastest
(defn fib-seq1 []
  (map #(% 0) (iterate (fn [[a b]] [b (+ a b)]) [1 1])))

;; pretty good
(defn fb2 []
  (map peek (iterate (fn [[a b]] [b (+ a b)]) [0 1])))


;; slower
(defn fb4 []
  (map first (iterate (fn [[a b]] (list b (+ a b))) '(1 1))))


;; slower
(defn fib-seq0 []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1])))

#_ (take 20 (fib-seq))
;=> (1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765)




;; But we could parameterize some of the things in the definition, like the first and second
;; elements (both 1s), and the operation to apply (+). We should be able to pass them as
;; arguments:

;; (fib-seq + 1 1) ;=> (1 1 2 3 5 8 13 ....)   That's your second task.

(defn fib-seq2 [op x y]
  (map first (iterate (fn [[a b]] [b (op a b)]) [x y])))





;; Your third task is more interesting: We don't have to limit ourselves to addition. In
;; fact, we should be able to use any function that takes two arguments of type T and
;; returns a value of T (closure property). Your task is to generate the first 10 elements
;; of the sequence (use take) for each of these combinations:

;; (take 10 (fib-seq str "X" "O"))
;; (take 10 (fib-seq * 1 1))
;; (take 10 (fib-seq * 1 2))
;; (take 10 (fib-seq * 1 -1))
;; (take 10 (fib-seq vector [] []))


;; @sw -- classic and probably best
(defn sw-fib-seq
  ([] (fib-seq + 1 1))
  ([f a b] (lazy-seq (cons a (fib-seq f b (f a b))))))




;; ----------------------------------------------------------------------

;; old idea from Alan Dipert but depends on def for recursive var, not a funtion call
;; (def afib (lazy-seq (cons 0 (reductions + 1 afib))))
;; note: had to swap arg order to get op to work in reductions
;; Not a good way to do it, but kind of amazing that it works!

(defn afib-seq
  ([] (afib-seq + 1 1))
  ([op a b] ((fn fibs [] (lazy-seq (cons a (reductions #(op %2 %) b (fibs))))))))
