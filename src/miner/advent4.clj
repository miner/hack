(ns miner.advent4)

;; Clojure Apropos webcast
;; https://www.youtube.com/watch?v=YdN58YxLXmU

;; example from Advent of Code
;; https://adventofcode.com/2019/day/4

;; Apropos Cast notes:
;; https://gist.github.com/apropos-cast/69ef76f3ec02642ddebe94827d5bb83d


;; You arrive at the Venus fuel depot only to discover it's protected by a password. The
;; Elves had written the password on a sticky note, but someone threw it out.
;; 
;; However, they do remember a few key facts about the password:
;; 
;; It is a six-digit number.
;; The value is within the range given in your puzzle input.
;; Two adjacent digits are the same (like 22 in 122345).
;; Going from left to right, the digits never decrease; they only ever increase or stay the
;;   same (like 111123 or 135679).
;;
;; Other than the range rule, the following are true:
;; 
;; 111111 meets these criteria (double 11, never decreases).
;; 223450 does not meet these criteria (decreasing pair of digits 50).
;; 123789 does not meet these criteria (no double).
;;
;; How many different passwords within the range given in your puzzle input meet these
;; criteria?


;; Eric said range was (168630 ... 718099)  Was that just for example?  Didn't seem like
;; part of the original problem.  His solution was to generate full range and filter.  I
;; suspect it's faster, to generate only the correct answers as you can prune as you go.


(defn digits [n]
  {:pre [(pos? n)]}
  (map #(- (long %) (long \0)) (seq (pr-str n))))

;; not so pretty
(defn rdd? [coll]
  (true? (reduce (fn [r x]
                   (if (= r x)
                     (reduced true)
                     x))
                 nil
                 coll)))


(defn double-digit? [coll]
  (loop [prev nil xs (seq coll)]
    (cond (empty? xs) false
          (= prev (first xs)) true
          :else (recur (first xs) (rest xs)))))

(defn non-decreasing? [coll]
  (loop [prev 0 xs (seq coll)]
    (cond (empty? xs) true
          (< (first xs) prev) false
          :else (recur (first xs) (rest xs)))))

(defn part1 []
  (let [rng (range 168630 718100)]
    (count (filter (every-pred double-digit? non-decreasing?)
                   (map digits rng)))))

(defn rn1 [rng]
  (filter (every-pred double-digit? non-decreasing?)
          (map digits rng)))

#_ (rn1 (range 100 399))
#_ ((1 1 1) (1 1 2) (1 1 3) (1 1 4) (1 1 5) (1 1 6) (1 1 7) (1 1 8) (1 1 9) (1 2 2) (1 3 3) (1 4 4) (1 5 5) (1 6 6) (1 7 7) (1 8 8) (1 9 9) (2 2 2) (2 2 3) (2 2 4) (2 2 5) (2 2 6) (2 2 7) (2 2 8) (2 2 9) (2 3 3) (2 4 4) (2 5 5) (2 6 6) (2 7 7) (2 8 8) (2 9 9) (3 3 3) (3 3 4) (3 3 5) (3 3 6) (3 3 7) (3 3 8) (3 3 9) (3 4 4) (3 5 5) (3 6 6) (3 7 7) (3 8 8))

#_ (count (rn1 (range 100 399)))
;; => 44



(defn part1r []
  (count (rn1 (range 168630 718100))))

;; part1 1686



(defn p1
  ([] (p1 10))
  ([n]
  (let [rng (range 168630 718100)]
    (take n (filter (every-pred double-digit? non-decreasing?)
                   (map digits rng))))))


;; part2 "has exactly a pair, not more in adjacent group"


;; similar to Apropos implementation
(defn isolated-pair1? [coll]
  (contains? (clojure.set/map-invert (frequencies coll)) 2))

;; suggestion to try partition-by identity instead of frequencies
;; slightly faster
(defn isolated-pair? [coll]
  (boolean (first (sequence (comp (partition-by identity) (filter #(= (count %) 2)))
                            coll))))

(defn part2 []
  (let [rng (range 168630 718100)]
    (count (filter (every-pred non-decreasing? isolated-pair?)
                   (map digits rng)))))

;; part2
;; 1145

;; slower
(defn iso-pair1? [coll]
  (loop [runs (partition-by identity coll)]
    (cond (empty? runs) false
          (= (count (first runs)) 2)  true
          :else (recur (rest runs)))))

      
(defn iso-pair2? [coll]
  (boolean (some #(= (count %) 2) (partition-by identity coll))))

;; slightly faster, reused above as my semi-official implementation
(defn iso-pair3? [coll]
  (boolean (first (sequence (comp (partition-by identity) (filter #(= (count %) 2)))
                            coll))))





(defn part2c []
  (let [rng (range 168630 718100)]
    (count (filter (every-pred non-decreasing? iso-pair3?)
                   (map digits rng)))))


(defn part2b []
  (let [rng (range 168630 718100)]
    (count (filter (every-pred non-decreasing? iso-pair2?)
                   (map digits rng)))))




(defn part2t1 []
  (count (into [] (comp (map digits) (filter non-decreasing?) (filter isolated-pair?))
               (range 168630 718100))))


(defn part2t2 []
  (count (sequence (comp (map digits) (filter non-decreasing?) (filter isolated-pair?))
                   (range 168630 718100))))

;; ugly constantly 1
(defn part2tc []
  (transduce (comp (map digits) (filter non-decreasing?) (filter isolated-pair?)
                   (map (constantly 1)))
             +
             0
             (range 168630 718100)))


(defn part2e []
  (reduce (fn [r _] (inc r))
          0
          (eduction (map digits) (filter non-decreasing?) (filter isolated-pair?)
                   (range 168630 718100))))



(defn index-of-decreasing [v]
  (loop [i 1]
    (cond (= i (count v)) nil
          (< (v i) (v (dec i)))  i
          :else (recur (inc i)))))

(defn index-of-last-unique [v]
  (case (count v)
    0 nil
    1 0
    (let [x (peek v)]
      (loop [i (count v)]
        (cond (zero? i) 0
              (= (v (dec i)) x) (recur (dec i))
              :else  i)))))


;;; gen from scratch
;;; digits 0-9
;;; can't start with 0?
;;; arbitrary start, but with desired number of digits
;;; arbitrary end, with same number of digits

;;; choose doubling digit 0-9
;;; walk across length (from start and end) as prefix-NN-suffix
;;; how many prefixes?  must be N or less (monotonically non-decreasing)
;;; how many suffixes?  must be N or more (monotonically non-decreasing)

;;; should be able to calculate and multiply as we walk indices

;; Length len
;; make sequential of len-1  d[0] .. d[len-1]
;;
;;  first digit 1-9    (but within start .. end)
;;  second digit d0-9
;;  third digit d1-9
;;
;; walk doubling across by inserting duplicate


;;; UNFINISHED
;; (defn next-non-decreasing [n]
;;   (let [digs (digits (inc n))]
;;     (if (non-decreasing? digs)
;;       digs
;; 
;; (defn next-non-decreasing [coll]
;;   (let [v (vec coll)
;;         i (index-of-non-decreasing v)]
;;     (cond (nil? i) nil ;; need to increment
;; 
;;         cnt (count v)]
;;     (loop [result [(first coll)] xs (rest coll)]
;;        (if (< (first xs) (peek result))
;;  
 
