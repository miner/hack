(ns miner.fizzbuzz)

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

(defn fb-combo [f b n]
  (cond (and f b) (str f b)
        f f
        b b
        :else n))

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


