(ns miner.fizzbuzz)



;; http://klipse.ghost.io/the-most-elegant-implementation-of-fizzbuzz/
;; my version of their FizzBuzz, one-based, with all strings
(defn kfb [n]
  (sequence (map (fn [f b n] (or (and f b (str f b)) f b (str n))))
            (cycle [nil nil "Fizz"])
            (cycle [nil nil nil nil "Buzz" ])
            (range 1 (inc n))))


;; I prefer 0-based, and numbers for numbers

(defn smfb [n]
  (sequence (map (fn [f b n] (or (and f b (str f b)) f b n)))
            (cycle ["Fizz" nil nil])
            (cycle ["Buzz" nil nil nil nil])
            (range n)))


;; (= (kfb 200) (map str (rest (smfb 201))))



;; Variation from Eric Normand's Challenge
;; https://gist.github.com/ericnormand/ef7ffcfb451e1a97a1ce54955e60c0c5
;;
;; NO CONDITIONALS: you're not going to write any conditionals or loops. Just to be clear,
;; that means no if, when, cond, or case expressions.  Write a function that is passed a
;; sequence of integers.  For each integer, if it's divisible by 3, print "Fizz". If it's
;; divisible by 5, print "Buzz". If it's divisible by both 3 and 5,
;; print "FizzBuzz". Finally, if it's divisible by neither, print the number itself. Each
;; printing should be on a separate line.


;; Is it cheating to use or/and as conditionals?  Gray area, but I don't think that should
;; be allowed.


(defn fb [n]
  (let [f (zero? (rem n 3))
        b (zero? (rem n 5))]
    (or (and f b "FizzBuzz")
        (and f "Fizz")
        (and b "Buzz")
        n)))

;; nested vectors are faster than nested maps with integer keys
(defn nfb [n]
  (-> [["FizzBuzz" "Buzz" "Buzz"]]
      (nth (mod n 5) ["Fizz"])
      (nth (mod n 3) n)))

#_  ;; my older version
(defn nfb0 [n]
  (-> [["FizzBuzz" "Fizz" "Fizz" "Fizz" "Fizz"] ["Buzz"] ["Buzz"]]
      (nth (mod n 3))
      (nth (mod n 5) n)))


;; all in one solution
(defn print-fizzbuzz [coll]
  (let [fb (fn [n]
             (-> [["FizzBuzz" "Buzz" "Buzz"]]
                 (nth (mod n 5) ["Fizz"])
                 (nth (mod n 3) n)))]
    (doseq [x (map fb coll)]
      (println x))))




;; Some implementers like to start at index 0, most start at 1.  For Clojure, it's natural
;; to start with 0.

;; http://c2.com/cgi/wiki?FizzBuzzTest
;; Write a program that prints the numbers from 1 to 100. But for multiples of three
;; print “Fizz” instead of the number and for the multiples of five print “Buzz”. For
;; numbers which are multiples of both three and five print “FizzBuzz”."


;; if you want to be one-based, you can use this
(defn range1 [n]
  ;; 1..n inclusive
  (range 1 (inc n)))

;; rem is actually a bit faster than mod (and = for non-neg)
(defn div? [x n] (zero? (mod x n)))

(defn fb1 [x]
  (cond (div? x 15) "fizzbuzz"
        (div? x 5) "buzz"
        (div? x 3) "fizz"
        :else x))

(defn fizzbuzz1 [num]
  (map fb1 (range num)))


;; Better version
;; inspired by http://clojure-and-me.blogspot.com/2012/08/functional-fizzbuzz.html

(defn fb-combo1 [f b n]
  (cond (and f b) (str f b)
        f f
        b b
        :else n))

(defn fb-combo2 [f b n]
  (if (and f b)
    (str f b)
    (or f b n)))

;; maybe a little harder to read, but fairly succinct
(defn fb-combo [f b n]
  (or (and f b (str f b)) f b n))

(defn fizzbuzz-cycle [num]
  (map fb-combo (cycle ["fizz" nil nil]) (cycle ["buzz" nil nil nil nil]) (range num)))

;; ----------------------------------------------------------------------


;; FizzBuzz without conditionals
;; some fun implmentations, zero-based
;; http://gigasquidsoftware.com/blog/2014/11/13/clojure-fizzbuzz-without-conditionals/

;; zero-based, natural, fast implementation
;; use for benchmarking
(defn fizzbuzz [n]
  (let [zmod (fn [^long m ^long d] (= 0 (rem m d)))
        ;; note: ^long on i is slower!
        fb (fn [i] (if (zmod i 3)
                     (if (zmod i 5)
                       "fizzbuzz"
                       "fizz")
                     (if (zmod i 5)
                       "buzz"
                       i)))]
    (map fb (range n))))


;; clever use of map access with default
;; @DerGuteMoritz
(defn dgm [n]
  (map #({0 "fizzbuzz"} (mod % 15) ({0 "fizz"} (mod % 3) ({0 "buzz"} (mod % 5) %)))
       (range n)))

;; zero-based, with transducers
(defn xfizzbuzz [^long n]
  (let [xfb-group (fn [size mark]
                    (comp (partition-all size) (map rest) (mapcat #(cons mark %))))]
    (sequence (comp (xfb-group 3 "fizz")
                    (xfb-group 5 "buzz")
                    (xfb-group 15 "fizzbuzz"))
              (range n))))

(defn xfizzbuzz1 [n]
  (sequence (comp (partition-all 3) (map rest) (mapcat #(cons "fizz" %))
                  (partition-all 5) (map rest) (mapcat #(cons "buzz" %))
                  (partition-all 15) (map rest) (mapcat #(cons "fizzbuzz" %)))
            (range n)))

(defn xfizzbuzz3 [n]
  (sequence (map (some-fn identity))
            (cycle (cons "fizzbuzz" (repeat 14 nil)))
            (cycle ["buzz" nil nil nil nil])
            (cycle ["fizz" nil nil])
            (range n)))


(defn xfizzbuzz4 [n]
  (sequence (map (fn [f b n] (if (or f b) (str f b) n)))
            (cycle ["fizz" nil nil])
            (cycle ["buzz" nil nil nil nil])
            (range n)))

(defn xfb15 [n]
  (sequence (map (fn [f b n] (if (or f b) (str f b) n)))
            (cycle [nil nil "Fizz"])
            (cycle [nil nil nil nil "Buzz" ])
            (range 1 (inc n))))


;;


(defn xfb1 [n]
  (sequence (comp (map list) (map #(some identity %)))
            (cycle (cons "fizzbuzz" (repeat 14 nil)))
            (cycle ["buzz" nil nil nil nil])
            (cycle ["fizz" nil nil])
            (range n)))


(defn xfb2 [n]
  (sequence (map #(some identity %&))
            (cycle (cons "fizzbuzz" (repeat 14 nil)))
            (cycle ["buzz" nil nil nil nil])
            (cycle ["fizz" nil nil])
            (range n)))


(defn xfb3 [n]
  (sequence (map (fn [a b c d] (or a b c d)))
            (cycle (cons "fizzbuzz" (repeat 14 nil)))
            (cycle ["buzz" nil nil nil nil])
            (cycle ["fizz" nil nil])
            (range n)))


(defn xfb [n]
  (sequence (map (some-fn identity))
            (cycle (cons "fizzbuzz" (repeat 14 nil)))
            (cycle ["buzz" nil nil nil nil])
            (cycle ["fizz" nil nil])
            (range n)))


(defn fb-natural [n]
  (let [zmod (fn [^long m ^long d] (= 0 (rem m d)))]
    (map (fn [i] (cond (zmod i 15) "fizzbuzz"
                       (zmod i 3) "fizz"
                       (zmod i 5) "buzz"
                       :else i))           
         (range n))))

;; my best so far
(defn best-fizzbuzz [n]
  (let [zmod (fn [^long m ^long d] (= 0 (rem m d)))
        ;; note: ^long on i is slower!
        fb (fn [i] (if (zmod i 3)
                     (if (zmod i 5)
                       "fizzbuzz"
                       "fizz")
                     (if (zmod i 5)
                       "buzz"
                       i)))]
    (map fb (range n))))

;; hinting didn't matter much
;; case didn't do any better


(defn fb-fast2 [n]
  (map (fn [^long i]
             (if (= 0 (rem i 3))
               (if (= 0 (rem i 5))
                 "fizzbuzz"
                 "fizz")
               (if (= 0 (rem i 5))
                 "buzz"
                 i)))
       (range n)))
