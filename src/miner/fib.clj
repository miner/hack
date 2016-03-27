;; http://nayuki.eigenstate.org/page/fast-fibonacci-algorithms
;; http://nayuki.eigenstate.org/res/fast-fibonacci-algorithms/fastfibonacci.java

;; See also the wikipedia page for interesting facts:
;; http://en.wikipedia.org/wiki/Fibonacci_number

(ns miner.fib
  (:refer-clojure))

(defn foo [x]
  (* 3 (+ x 2)))

;; literal translation
(defn fast-fib-literal-translation [n]
  (loop [i 31 a 0 b 1 m 0]
    ;; invariant:  a = F(m), b = F(m+1)
    (if (neg? i)
      a
      (let [d (* a (- (bit-shift-left b 1) a))
            e (+ (* a a) (* b b))
            a d
            b e
            m (* m 2)]
        (if-not (zero? (bit-and n (bit-shift-left 1 i)))
          (let [c (+ a b)
                a b
                b c
                m (inc m)]
            (recur (dec i) a b m))
          (recur (dec i) a b m))))))

;; fast-fib is correct but overflows around n=91
;; simplified from above
(defn fast-fib [n]
  ;; (assert (<= n 90))
  (loop [i 31 a 0 b 1 m 0]
    ;; invariant:  a = F(m), b = F(m+1)
    (if (neg? i)
      a
      (let [d (* a (- (* 2 b) a))
            e (+ (* a a) (* b b))]
        (if (bit-test n i)
          (recur (dec i) e (+ d e) (inc (* 2 m)))
          (recur (dec i) d e (* 2 m)))))))

;; using overflow protection (might return BigInt)
(defn fast-fib1 [n]
  (loop [i 31 a 0 b 1 m 0]
    ;; invariant:  a = F(m), b = F(m+1)
    (if (neg? i)
      a
      (let [d (*' a (-' (* 2 b) a))
            e (+' (*' a a) (*' b b))]
        (if (bit-test n i)
          (recur (dec i) e (+' d e) (inc (*' 2 m)))
          (recur (dec i) d e (*' 2 m)))))))


;; SEM -- you should try doing a trampoline version of fib2???

;; fast but it's recursive!
(defn fib2 [n]
  ;; return vector [f(n), f(n+1)]
  (if (zero? n)
    [0 1]
    (let [[a b] (fib2 (quot n 2))
          c (* a (- (* b 2) a))
          d (+ (* a a) (* b b))]
      (if (even? n)
        [c d]
        [d (+ c d)]))))


(defn fib1 [n]
  (first (fib2 n)))



(defn fibr [n]
  ;; return vector [f(n), f(n+1)]
  (loop [a 0 b 1 n n]
    (if (zero? n)
      [a b]
      (let [[a b] (fib2 (quot n 2))
            c (* a (- (* b 2) a))
            d (+ (* a a) (* b b))]
        (if (even? n)
          (recur c d (quot n 2))
          (recur d (+ c d) (quot n 2)))))))


;; Oops, there are some semi-mistakes in this.
;; zero? also tests against 0.0 (unnecessary), use = 0 if guaranteed long
;; == also tests against floating-point (unnecessary), use = if all longs
;; (You may have been confused by Java == for numbers)

;; good enough base to test against
(defn slow-fib [n] 
  (cond (zero? n) 0
        (== n 1) 1 
        :else (+ (slow-fib (- n 2)) (slow-fib (dec n)))))

;; better basic definition
(defn basic-fib [n] 
  (case n
    0 0
    1 1
    (+ (basic-fib (- n 2)) (basic-fib (dec n)))))


;; cache as a sequence and take nth when needed

(defn next-terms [term-1 term-2] 
  (let [term-3 (+ term-1 term-2)] 
    (lazy-seq  
      (cons term-3  
            (next-terms term-2 term-3)))))

(defn fibonacci [t1 t2] 
  (concat [t1 t2]  
          (next-terms t1 t2))) 

(defn infinite-fib-sequence []
  (fibonacci 0 1))
			      
(comment 

(take 15 (fibonacci 0 1)) 

)

;; from @ghoseb
(def fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

#_ (take 15 fibs)


(comment
;; I like to have the zero-eth value be 0, but the normal def starts with 1, 1, ...
(map basic-fib (range 20))
(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)

)


;; http://en.literateprograms.org/Fibonacci_numbers_(Python)#Direct_computation_of_the_nth_Fibonacci_number_with_Binet.27s_formula

;; also http://mathworld.wolfram.com/BinetsFibonacciNumberFormula.html

;; very fast direct calculation but wrong after n > 70 due to rounding errors
(defn binet-fib [n]
  ; (assert (<= n 70))
  (let [phi (/ (+ 1.0 (Math/sqrt 5.0)) 2.0)]
    (long (Math/round (/ (- (Math/pow phi n) (Math/pow (- 1.0 phi) n))
                         (Math/sqrt 5.0))))))
        

;; For reference
;; http://www.miniwebtool.com/list-of-fibonacci-numbers/?number=100
(def fib100 [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 17711 28657
             46368 75025 121393 196418 317811 514229 832040 1346269 2178309 3524578 5702887 9227465
             14930352 24157817 39088169 63245986 102334155 165580141 267914296 433494437 701408733
             1134903170 1836311903 2971215073 4807526976 7778742049 12586269025 20365011074 32951280099
             53316291173 86267571272 139583862445 225851433717 365435296162 591286729879 956722026041
             1548008755920 2504730781961 4052739537881 6557470319842 10610209857723 17167680177565
             27777890035288 44945570212853 72723460248141 117669030460994 190392490709135
             308061521170129 498454011879264 806515533049393 1304969544928657 2111485077978050
             3416454622906707 5527939700884757 8944394323791464 14472334024676221 23416728348467685
             37889062373143906 61305790721611591 99194853094755497 160500643816367088 259695496911122585
             420196140727489673 679891637638612258 1100087778366101931 1779979416004714189
             2880067194370816120 4660046610375530309 7540113804746346429 12200160415121876738
             19740274219868223167 31940434634990099905 51680708854858323072 83621143489848422977
             135301852344706746049 218922995834555169026] )



