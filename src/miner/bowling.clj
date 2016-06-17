(ns miner.bowling)

;; http://codingdojo.org/cgi-bin/index.pl?KataBowling
;; https://www.javacodegeeks.com/2016/05/bowling-kata-clojure-f-scala.html

;; game is a string, encoding balls rolled
;; X for strike
;; / for spare
;; - for a miss or gutter ball
;; 1-9 for that many pins

;; If last frame is a strike or spare, the extra balls only accrue to that frame.

;; A "ball" re-encodes the character as 0 to 10 or :spare.  This is for readability and
;; more convenient scoring.

(defn ch->ball [ch]
  (case ch
    \X 10
    \/ :spare
    \- 0
    (- (long ch) (long \0))))

(defn ball-seq [game]
  (map ch->ball (remove #(= % \space) game)))

;; Note, a :spare is scored only on the second ball, so we ignore the ball A if the
;; next B is a :spare.  By the way, it's illegal to have two :spare in a row or a :spare
;; immediately after a strike (10) but we don't check.

;; (A B C) are three sequential balls, triplet-val returns the score assigned for A.
(defn triplet-score [[a b c]]
  (case a
    :spare (+ 10 b)
    10 (case c
         :spare 20
         (+ 10 b c))
    (case b
      :spare 0
      a)))

(defn ten-frames [balls]
  (loop [balls balls cnt 0 halves 0]
    (if (or (= halves 20) (empty? balls))
      cnt
      (recur (rest balls) (inc cnt) (if (= (first balls) 10) (+ halves 2) (inc halves))))))

;; Each ball might need up to two additional balls to calculate a score so we partition into
;; triplets.  We ignore the extraneous triplets that come from extra balls used only to
;; score a final frame strike or spare.

(defn score [game]
  (let [balls (ball-seq game)
        tenframes (ten-frames balls)]
    (reduce + (map triplet-score (take tenframes (partition-all 3 1 balls))))))

(defn smoke-test []
  (and (= (score "35 6/ 7/ X 45 X X X XXXX") 223)
       (= (score "11 11 11 11 11 11 11 11 X 11") 30)
       (= (score "XXXXXXXXXXXX") 300)
       (= (score "9-9-9-9-9-9-9-9-9-9-") 90)
       (= (score "5/5/5/5/5/5/5/5/5/5/5") 150)))
