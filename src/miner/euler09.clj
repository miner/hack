(ns miner.euler09)

;;; https://clojure-diary.gitlab.io/2024/11/13/project-euler-problem-9.html

;; euler_9.clj

(def possibilities
  (for [a (range 1 1001) b (range (+ a 1) 1001)]
    [a b]))

(defn satisfies-condition [possibility]
  (let [a (first possibility)
        b (second possibility)
        c (- 1000 (+  a b))]
    (== (+ (* a a) (* b b)) (* c c))))

;; (filter satisfies-condition possibilities)

(defn euler09 []
  (let [[a b] (first (filter satisfies-condition possibilities))
        c (- 1000 (+ a b))]
    (* a b c)))

;;; 31875000


;;; https://projecteuler.net/problem=9
;;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;;; a^2 + b^2 = c^2   For example, [3, 4 5]
;;;
;;; There exists exactly one Pythagorean triplet for which their sum is 1000.  Find the
;;; product abc.


;;; SEM -- much faster to check conditions in `for`.  Note a < b < c so the :while cuts off
;;; a lot of the search space.

;;; Solution a=200  b=375 c=425  abc=31875000

(defn eul9 []
  (first
   (for [a (range 1 1000)
         b (range (inc a) 1000)
         :let [c (- 1000 a b)]
         :while (> c b)
         :when (= (+ (* a a) (* b b)) (* c c))]
     (* a b c))))


;;; Given a+b+c=1000 and a<b<c so a < (/ 1000 3), b < (/ 1000 2) so we can restrict the
;;; ranges safely.  However, you don't really save anything as the original search
;;; terminates before the tighter limits would have helped.

(defn eul9a []
  (first
   (for [a (range 1 333)
         b (range (inc a) 500)
         :let [c (- 1000 a b)]
         :while (> c b)
         :when (= (+ (* a a) (* b b)) (* c c))]
     (* a b c))))
