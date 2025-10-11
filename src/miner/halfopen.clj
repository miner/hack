(ns miner.halfopen)

;;; https://zayenz.se/blog/post/how-to-check-for-overlapping-intervals/

;;; A half-open interval of integers is represented as a Clojure vector of [start, end]
;;; wnere end is exclusive.  This follows the usual convention in Java and Clojure for
;;; ranges and string indices.

;;; [start, open-end], empty interval not allowed (for now), min length is 1

(defn start [hoi]
  (nth hoi 0))

(defn end [hoi]
  (nth hoi 1))

(defn hoi? [x]
  (and (vector? x)
       (= (count x) 2)
       (< (start x) (end x))))

(defn length [hoi]
  (- (end hoi) (start hoi)))

(defn overlap? [hoi1 hoi2]
  (and (< (start hoi2) (end hoi1))
       (< (start hoi1) (end hoi2))))

(defn in? [hoi x]
  (and (>= x (start hoi))
       (< x (end hoi))))

(defn hoi-seq [hoi]
  (range (start hoi) (end hoi)))

(defn hoi-rseq [hoi]
  (range (dec (end hoi)) (dec (start hoi)) -1))


(defn covers? [hoi subhoi]
  (and (<= (start hoi) (start subhoi))
       (<= (end subhoi) (end hoi))))


;;; return new hoi that covers both args
(defn cover [hoi1 hoi2]
  (vector (min (start hoi1) (start hoi2))
          (max (end hoi1) (end hoi2))))

(def fff [3 7])
(def hhh [3 12])
(def ggg [10 20])
(def iii [20 25])


(defn smoke-hoi []
  (assert (overlap? hhh ggg))
  (assert (not (overlap? ggg iii)))
  (assert (not (overlap? hhh iii)))
  (assert (= (filter #(in? hhh %) (range 20)) (hoi-seq hhh)))
  (assert (covers? hhh fff))
  (assert (not (covers? hhh ggg)))
  true)
