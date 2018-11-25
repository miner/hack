;; http://nayuki.eigenstate.org/page/fast-fibonacci-algorithms
;; http://nayuki.eigenstate.org/res/fast-fibonacci-algorithms/fastfibonacci.java

;; See also the wikipedia page for interesting facts:
;; http://en.wikipedia.org/wiki/Fibonacci_number

;; https://lee-phillips.org/lispmath/
;;



;; See also (just moved the original)
;; https://www.nayuki.io/page/fast-fibonacci-algorithms
;; https://news.ycombinator.com/item?id=12871156

;; SEM:  I like starting the Fibonacci sequence with 0
;; [0 1 1 2 3 5 8 13 21 34 55 89 ...]
;;
;; Many people prefer starting with 1.  These code samples start with 0.  You can always
;; drop the first zero to get a 1-based solution.
;;
;; *unchecked-math* :warn-on-boxed makes things faster and warns when Clojure has to box
;; primitives.  However, some of the calculations will overflow longs at about (fib 93) so
;; to be safe, don't go over 90 unless you use checked math and bignums.

;; By definition,  F(0) = 0, F(1) = 1, F(i+2) = F(i) + F(i+1)
;; Let, a = F(i), b = F(i+1)
;;
;; Key insight that makes some implementations faster, known as "fast doubling":
;;     F(2i) = a * (2b - a)
;;     F(2i+1) = a^2 + b^2
;;
;; so you can skip by powers of 2 rather than calculating each step.

;;; 09/18/17  17:20 by miner -- learned something new (to me):
;; "A positive integer n is a Fibonacci number if and only if either (5n^2 – 4)
;; or (5n^2 + 4) is a perfect square."  More in post:
;; https://www.johndcook.com/blog/2017/02/11/inverse-fibonacci-numbers/


;; The obvious best "engineering" implementation is at the bottom, called `lookup-fib`. 
;; The rest of this file is mostly for fun.

(ns miner.fib
  (:refer-clojure)
  (:require [primitive-math :as p]))


(def save-unchecked-all *unchecked-math*)
(set! *unchecked-math* :warn-on-boxed)



;; Classic fib, but gets slow because recursion uses too much stack
(defn fibc ^long [^long n]
  ;; assumes not-neg n
  (if (< n 2)
    n
    (+ (fibc (- n 2)) (fibc (- n 1)))))



;; Memoizing is a common approach to caching results that are likely to be needed again, but
;; increases memory costs.
(declare memo-fib)

(defn mfib [^long n]
  ;; assumes not-neg n
  (if (< n 2)
    n
    (+ ^long (memo-fib (- n 2)) ^long (memo-fib (- n 1)))))

(def memo-fib (memoize mfib))



;; Simple and reasonable with growing vector, returns N elements
(defn nfibs [^long n]
  (if (< n 2)
    (vec (range (inc n)))
    (reduce  (fn [v ^long i]
               (conj v (+ ^long (nth v (dec i))
                          ^long (nth v (dec (dec i))))))
             [0 1]
             (range 2 n))))

(defn pfibs [^long n]
  (if (< n 2)
    (vec (range (inc n)))
    (reduce  (fn [v ^long i]
               (conj v (+ ^long (peek v)
                          ^long (peek (pop v)))))
             [0 1]
             (range 2 n))))


;; returns exactly Nth element
(defn nfib [^long n]
  (if (< n 2)
    n
    (peek (reduce  (fn [v ^long i]
               (conj v (+ ^long (nth v (dec i))
                          ^long (nth v (dec (dec i))))))
             [0 1]
             (range 2 (inc n))))))

(defn pfib [^long n]
  (if (< n 2)
    n
    (peek (reduce  (fn [v ^long i]
               (conj v (+ ^long (peek v)
                          ^long (peek (pop v)))))
             [0 1]
             (range 2 (inc n))))))




;; The original algorithm was for 32-bit int N, which is more than enough for most people,
;; but technically Clojure uses 64-bit long integers so I want to note that here.

;; SEM: BUG??? What's M used for?  Did you miss something about M vs. N for termination?
;; Seems like M is just the new I for the "doubling" and is just book-keeping in case you
;; need to verify it.

;; literal translation
(defn fast-fib-literal-translation [^long n]
  (loop [i 31 a 0 b 1 m 0]
    ;;(assert (< n Integer/MAX_VALUE))
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
;; updated to handle long n, but really i can't be more than 8 because of overflow

(defn fast-fib [^long n]
  ;; (assert (< n 93))
  (loop [i (- 63 (Long/numberOfLeadingZeros n)) a 0 b 1]
    ;; invariant:  a = F(m), b = F(m+1)
    (if (neg? i)
      a
      (let [d (* a (- (* 2 b) a))
            e (+ (* a a) (* b b))]
        (if (bit-test n i)
          (recur (dec i) e (+ d e))
          (recur (dec i) d e))))))


(defn prim-fib [^long n]
  ;; (assert (< n 93))
  (loop [i (p/- 63 (Long/numberOfLeadingZeros n)) a 0 b 1]
    ;; invariant:  a = F(m), b = F(m+1)
    (if (p/< i 0)
      a
      (let [d (p/* a (p/- (p/* 2 b) a))
            e (p/+ (p/* a a) (p/* b b))]
        ;; SEM hack to get primitive bit-test but opposite sense so if-not
        (if-not (p/zero? (p/bit-and n (p/bit-shift-left 1 i)))
          (recur (p/dec i) e (p/+ d e))
          (recur (p/dec i) d e))))))





(def save-unchecked *unchecked-math*)
(set! *unchecked-math* false)

(def save-reflection *warn-on-reflection*)
(set! *warn-on-reflection* false)

;; using overflow protection (might return BigInt)
;; that means loop params have to be boxed, which is slower than primitives
(defn big-fib [^long n]
  (loop [i (- 63 (Long/numberOfLeadingZeros n)) a 0 b 1]
    ;; invariant:  a = F(m), b = F(m+1)
    (if (neg? i)
      a
      (let [d (*' a (-' (*' 2 b) a))
            e (+' (*' a a) (*' b b))]
        (if (bit-test n i)
          (recur (dec i) e (+' d e))
          (recur (dec i) d e))))))

(set! *warn-on-reflection* save-reflection)
(set! *unchecked-math* save-unchecked)

;; SEM -- you should try doing a trampoline version of fib2???

;; fast but it's recursive!
(defn fib2 [^long n]
  ;; return vector [f(n), f(n+1)]
  (if (zero? n)
    [0 1]
    (let [[^long a ^long b] (fib2 (quot n 2))
          c (* a (- (* b 2) a))
          d (+ (* a a) (* b b))]
      (if (even? n)
        [c d]
        [d (+ c d)]))))


(defn fib1 [n]
  (first (fib2 n)))


;; pretty good but it got a bit better below
(defn fib-nr [^long n]
  ;; (assert (< n 93))
  (if (pos? n)
    (loop [a 1 b 1 i 1]
      (cond (= i n) a
            (<= (* i 2) n) (recur (* a (- (* b 2) a))
                                  (+ (* a a) (* b b))
                                  (* i 2))
            :else (recur b (+ a b) (inc i))))
    0))


;; Fastest, not prettiest.  (Well, `lookup-fib` is still faster, of course.)
(defn myfib [^long n]
  ;; (assert (< n 93))
  (if (< n 2)
    n
    (loop [a 1 b 2 i 2]
      (if (<= (* i 2) n)
        (recur (* a (- (* b 2) a))
               (+ (* a a) (* b b))
               (* i 2))
        (if (= n i)
          a
          (loop [a a b b i (inc i)]
            (if (= n i) b (recur b (+ a b) (inc i)))))))))


;; SEM: new idea, you can run down from a power of two, not just up
(defn ufib [^long n]
  ;; (assert (< n 93))
  (if (< n 2)
    n
    (loop [a 1 b 2 i 2]
      (cond (<= (* i 2) n) (recur (* a (- (* b 2) a))
                                  (+ (* a a) (* b b))
                                  (* i 2))
            (= i n) a
            (< (- (* i 2) n) (- n i)) (loop [c (* a (- (* b 2) a))
                                             d (+ (* a a) (* b b))
                                             i (* i 2)]
                                        (if (= n i) c (recur (- d c) c (dec i))))
            :else (loop [a a b b i (inc i)]
                    (if (= n i) b (recur b (+ a b) (inc i))))))))



;; prim is about the same
(defn pmyfib [^long n]
  ;; (assert (< n 93))
  (if (p/< n 2)
    n
    (loop [a 1 b 2 i 2]
      (if (p/<= (* i 2) n)
        (recur (p/* a (p/- (p/* b 2) a))
               (p/+ (p/* a a) (p/* b b))
               (p/* i 2))
        (if (p/== n i)
          a
          (loop [a a b b i (p/inc i)]
            (if (p/== n i) b (recur b (p/+ a b) (p/inc i)))))))))


;; https://news.ycombinator.com/item?id=6954218
;; https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%25_sec_1.2.4

;; translated from SICP
(defn fib-sicp [^long n]
  (loop [a 1 b 0 p 0 q 1 count n]
    (cond (zero? count) b
          (even? count) (recur a
                               b
                               (+ (* p p) (* q q))
                               (+ (* 2 p q) (* q q)) 
                               (quot count 2))
          :else (recur (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (dec count)))))





(defn plan2 [^long n]
  (loop [n n stack ()]
    (cond (zero? n) stack
          (odd? n) (recur (quot n 2) (conj stack 1))
          :else (recur (quot n 2) (conj stack 2)))))

;; works but not fast enough to be worth complexity, about 3x fib2-nr
(defn fibp [n]
  (let [plan (plan2 n)]
    (loop [ps plan a 0 b 1]
      (if (seq ps)
        (let [c ^long (* a (- (* b 2) a))
              d ^long (+ (* a a) (* b b))
              p (long (first ps))]
          (case p
            1 (recur (rest ps) d (+ c d))
            2 (recur (rest ps) c d)))
        a))))


;; cache as a sequence and `take` or  `nth` when needed

(defn next-terms [^long term-1 ^long term-2] 
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
(take 15 (infinite-fib-sequence))
)


;;; lazy, caching versions, use `take` to get what you want
;;; clever but never release the cached values

;; from Christophe Grand many years ago on the mailing list.
;; also attributed to @ghoseb
(def cgfibs (map first (iterate (fn [[^long a ^long b]] [b (+ a b)]) [0 1])))

;; project Euler #2
(defn euler2 []
  (transduce (comp (take-while #(< ^long % 4000000)) (filter even?)) + 0 cgfibs))


#_ (take 15 cgfibs)


;; from Alan Dipert
(def afib (lazy-seq (cons 0 (reductions + 1 afib))))



(comment
;; I like to have the zero-eth value be 0, but the normal def starts with 1, 1, ...
(map basic-fib (range 20))
(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)

)


;; http://en.literateprograms.org/Fibonacci_numbers_(Python)#Direct_computation_of_the_nth_Fibonacci_number_with_Binet.27s_formula

;; also http://mathworld.wolfram.com/BinetsFibonacciNumberFormula.html

;; But not as fast as myfib!

;; very fast direct calculation but wrong after n > 70 due to rounding errors
(defn binet-fib [n]
  ; (assert (<= n 70))
  (let [phi (/ (+ 1.0 (Math/sqrt 5.0)) 2.0)]
    (long (Math/round (/ (- (Math/pow phi n) (Math/pow (- 1.0 phi) n))
                         (Math/sqrt 5.0))))))


;; slight refactoring, not much diff
(defn binet-fib2 [n]
  ; (assert (<= n 70))
  (let [phi1 (/ (Math/sqrt 5.0) 2.0)]
    (long (Math/round (/ (- (Math/pow (+ 0.5 phi1) n) (Math/pow (- 0.5 phi1) n))
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

;; after #92, the values are larger than long can hold so the go to bignums
(def fib93 (subvec fib100 0 93))


;; The best engineering solution is to use a vector of known values.
(defn lookup-fib ^long [^long n]
  ;; (assert (< n 100))
  (fib93 n))


(defn test-fib [f]
  (if (= fib93 (map f (range 93)))
    true
    (println
     (take 5 (partition 3 (mapcat (fn [n] (let [a (lookup-fib n) b (f n)]
                                            (when (not= a b) [n a b])))
                      (range 93)))))))







(comment
  
(drop 50 (take 71 (infinite-fib-sequence)))
(12586269025 20365011074 32951280099 53316291173 86267571272 139583862445 225851433717
365435296162 591286729879 956722026041 1548008755920 2504730781961 4052739537881
6557470319842 10610209857723 17167680177565 27777890035288 44945570212853 72723460248141
117669030460994 190392490709135)

(map binet-fib (range 50 71))

(20365011074 32951280099 53316291173 86267571272 139583862445 225851433717 365435296162
591286729879 956722026041 1548008755920 2504730781961 4052739537881 6557470319842
10610209857723 17167680177565 27777890035288 44945570212853 72723460248141 117669030460994
190392490709135)

)



(set! *unchecked-math* save-unchecked-all)


;; Fibonacci numbers are related to Lucas numbers
;; https://en.wikipedia.org/wiki/Lucas_number

(defn lucas [^long n]
  (case n
    0 2
    1 1
    (+ (myfib (dec n)) (myfib (inc n)))))





;; very old thread on mailing list
;; https://groups.google.com/forum/#!topic/clojure/V6EvGg2rXhs

(defn fib 
  ([n] 
     (fib n 1)) 
  ([n b] 
     (if (zero? b)                        ;calculate lucas numbers 
       (cond 
        (zero? n) 2 
        (= n 1) 1 
        :otherwise 
        (if (even? n) 
          (let [ k (/ n 2) 
                 l (fib k 0)] 
            (+ (* l l) (if (even? k) -2 2))) 
          (let [ k (- n 1) 
                 l (fib k 0) 
                 f (fib k 1)] 
            (/ (+ (* 5 f) l) 2)))) 
       (cond                                ;calculate fibonacci numbers 
        (zero? n) 0 
        (= n 1) 1 
        :otherwise 
        (if (even? n) 
          (let [ k (/ n 2) 
                 l (fib k 0) 
                 f (fib k 1)] 
            (* f l)) 
          (let [ k (- n 1) 
                 l (fib k 0) 
                 f (fib k 1)] 
            (/ (+ f l) 2))))))) 
