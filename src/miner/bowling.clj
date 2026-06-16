(ns miner.bowling
  (:require [clojure.string :as str]))

;;; Years later, I discovered that Uncle Bob and Stuart Halloway worked this problem.
;;; https://github.com/stuarthalloway/clojure-bowling

;;; My final score is much faster and simpler than my previous efforts.  I spent too much
;;; effort trying to get the triplet style scoring to work as in score1 and friends.

;;; There's no particular advantage to being lazy for a small program like this so I think
;;; it makes sense to be eager -- using into and reduce-kv.


;;; SEE ALSO my transmuters project for a variation (but not really better).

;; https://codingdojo.org/kata/Bowling/
;; https://www.javacodegeeks.com/2016/05/bowling-kata-clojure-f-scala.html

;; game is a string, encoding balls rolled
;; X for strike
;; / for spare
;; - for a miss or gutter ball
;; 1-9 for that many pins
;; spaces are ignored

;; If last frame is a strike or spare, the extra balls only accrue to that frame.



;;; Main idea: use a vector to hold all the balls rolled so it's convenient to access the
;;; next two balls for scoring spares and strikes.  Using reduce-kv gives us the index i of
;;; the current ball b.  (That's faster than using map-indexed.)  Keep frame count `fc`,
;;; counting down from 20 half-frames to 0.  When fc is even, the ball is the first of a
;;; frame.  We score the frame only after the second ball, except for a strike which counts
;;; as a full frame (-2).  Counting down is slightly faster than counting up.  The odd/even
;;; scheme eliminates the need for a frame marker (nil in some of my other implementations)
;;; or keeping a vector of frames which is a bit slower than doing the running total score.
;;;
;;; Note: str/replace is much faster than (remove #{\space}).
;;; We convert the characters to the appropriate numbers for convenient scoring.  It is a
;;; happy accident that \ (spare) maps into -1 so we can test for a spare mark with neg?.
;;; Strikes are simply encoded as a 10.

;;; The new champ!

(defn score [game]
  (let [bv (mapv (fn [c] (let [x (- (long c) (long \0))] (case x 40 10 -3 0 x)))
                 (str/replace game " " ""))]
    ;; note X 40, \ -1, - -3.  The test for a spare mark is neg?
    (peek
     (reduce-kv (fn [[fc sc] i b]
                  (if (zero? fc)
                    (reduced [sc])
                    (if (even? fc)
                      ;; first ball of frame
                      (if (= b 10) ;strike
                        (let [b2 (bv (+ i 2))]
                          [(- fc 2)
                           (if (neg? b2) (+ sc 20) (+ sc 10 (bv (inc i)) b2))])
                        ;; don't score first ball, we will account for it on second ball
                        [(dec fc) sc])
                      ;; second ball of frame, recover the previous ball if necessary
                      [(dec fc)
                       (if (neg? b) (+ sc 10 (bv (inc i))) (+ sc (bv (dec i)) b))])))
                ;; init at 20 half-frames, and zero score
                [20 0]
                bv))))



;;; ----------------------------------------------------------------------



;; A "ball" re-encodes the character as 0 to 10 or :spare.  This is for readability and
;; more convenient scoring.  (BTW, my final best approach switched to -1 for the spare mark.)

(defn ch->ball [ch]
  (case ch
    \X 10
    \/ :spare
    \- 0
    (- (long ch) (long \0))))


;;; this was pretty good
(defn score11 [game]
  (let [bv (into [] (comp (remove #(= % \space)) (map ch->ball)) game)]
    (peek
     (reduce-kv (fn [[fc sc] i b]
                  (if (zero? fc)
                    (reduced [sc])
                    (if (even? fc)
                      ;; first ball of frame
                      (if (= b 10) ;strike
                        (let [b2 (bv (+ i 2))]
                          [(- fc 2)
                           (if (= b2 :spare) (+ sc 20) (+ sc 10 (bv (inc i)) b2))])
                        ;; don't score first ball, we will account for it on second ball
                        [(dec fc) sc])
                      ;; second ball of frame, recover the previous ball if necessary
                      [(dec fc)
                       (if (= b :spare) (+ sc 10 (bv (inc i))) (+ sc (bv (dec i)) b))])))
                ;; init at 20 half-frames, and zero score
                [20 0]
                bv))))


;;; The transducer version has to use map-indexed to get i which is a bit slower.

;;; Much faster than older, but no longer best.
;;; using nil as the new frame marker, otherwise the (peek fv) is the first ball of the frame.
;;; basically, goes to 11 frames, but always pop last nil before adding up. Only counting
;;; 10 frames.

;;; was bugby when I tried to put pop into reduced expr because sometimes the last ball is
;;; just enough to finish.  In which case, it doesn't terminate early on the count fv.  The
;;; outside pop is necessary to clear the placeholder nil as done correcly in score.  The
;;; check on the frame count also protects access to later balls.  Assuming legal games.


(defn score2 [game]
  (let [bv (into [] (comp (remove #(= % \space)) (map ch->ball)) game)]
    (reduce + 0
            (pop
             (reduce-kv (fn [fv i b]
                          (if (= (count fv) 11)
                            (reduced fv)
                            (let [fr (peek fv)
                                  pfv (pop fv)]
                              (if (nil? fr)
                                (if (= b 10) ;strike
                                  (let [b2 (bv (+ i 2))]
                                    (if (= b2 :spare)
                                      (conj pfv 20 nil)
                                      (conj pfv (+ 10 (bv (inc i)) b2) nil)))
                                  (conj pfv b))
                                (if (= b :spare)
                                  (conj pfv (+ 10 (bv (inc i))) nil)
                                  (conj pfv (+ fr b) nil))))))
                        [nil]
                        bv)))))



;; slightly faster to avoid conj/3
(defn score3 [game]
  (let [bv (into [] (comp (remove #(= % \space)) (map ch->ball)) game)]
    (reduce + 0
            (pop
             (reduce-kv (fn [fv i b]
                          (if (= (count fv) 11)
                            (reduced fv)
                            (let [fr (peek fv)
                                  pfv (pop fv)]
                              (if (nil? fr)
                                (if (= b 10) ;strike
                                  (let [b2 (bv (+ i 2))]
                                    (if (= b2 :spare)
                                      (conj (conj pfv 20) nil)
                                      (conj (conj pfv (+ 10 (bv (inc i)) b2)) nil)))
                                  (conj pfv b))
                                (if (= b :spare)
                                  (conj (conj pfv (+ 10 (bv (inc i)))) nil)
                                  (conj (conj pfv (+ fr b)) nil))))))
                        [nil]
                        bv)))))

;; transduce with map-indexed not faster, but not bad

;;; no advantage to doing the map-indexed up front into the bv.



;;; Older, less good efforts below.


(defn ball-seq [game]
  (map ch->ball (remove #{\space} game)))

;; Note, a :spare is scored only on the second ball, so we ignore the ball A if the
;; next B is a :spare.  By the way, it's illegal to have two :spare in a row or a :spare
;; immediately after a strike (10) but we don't check.

;; (A B C) are three sequential balls, triplet-score returns the score assigned for A.
(defn triplet-score [[a b c]]
  (case a
    :spare (+ 10 b)
    10 (case c
         :spare 20
         (+ 10 b c))
    (case b
      :spare 0
      a)))


;; ten-frames counts the balls for an actual game of ten frames.  It could be up to 20
;; "half-frames", but might be less as strikes need special treatment.  This count tells us
;; how many triplets to score.

(defn ten-frames [balls]
  (loop [bs balls cnt 0 halves 0]
    (cond 
        (= halves 20) cnt
        (empty? bs) (throw (ex-info "Incomplete sequence of bowling balls" {:balls balls}))
        :else (recur (next bs) (inc cnt) (if (= (first bs) 10) (+ halves 2) (inc halves))))))


;; Each ball might need up to two additional balls to calculate a score so we partition into
;; triplets.  We ignore the extraneous triplets that come from extra balls used only to
;; score a final frame strike or spare.

(defn score1 [game]
  (let [balls (ball-seq game)
        tenframes (ten-frames balls)]
    (reduce + (map triplet-score (take tenframes (partition-all 3 1 balls))))))

(defn smoke-test
  ([] (smoke-test score))
  ([score]
   (assert (= (score "35 6/ 7/ X 45 X X X XXXX") 223))
   (assert (= (score "11 11 11 11 11 11 11 11 X 11") 30))
   (assert (= (score "11 11 11 11 11 11 11 11 11 X 11") 30))
   (assert (= (score "XXXXXXXXXXXX") 300))
   (assert (= (score "9-9-9-9-9-9-9-9-9-9-") 90))
   (assert (= (score "5/5/5/5/5/5/5/5/5/5/5") 150))
   (assert (= (score "12 12 12 12 12 12 12 12 12 0/X") 47))
   (assert (= (score "XX 3/ 4/ X 54 7/ X X X 3/")  198))
   (assert (= (score "XX 3/ 4/ X 54 7/ X X X 37")  198))
   true))


(def games ["35 6/ 7/ X 45 X X X XXXX"
            "11 11 11 11 11 11 11 11 X 11"
            "11 11 11 11 11 11 11 11 11 X 11"
            "XXXXXXXXXXXX"
            "XX 3/ 4/ X 54 7/ X X X 3/"
            "XX 3/ 4/ X 54 7/ X X X 37"
            "9-9-9-9-9-9-9-9-9-9-"
            "5/5/5/5/5/5/5/5/5/5/5"
            "12 12 12 12 12 12 12 12 12 0/X"])


(def shgs ["XXXX"
            "11 X 11"
            "9-9-"
            "5/5/5"
            "12 0/X"])



(defn score5 [game]
  (let [ballv (into [] (comp (remove #{\space}) (map ch->ball)) game)
        tenframes (ten-frames ballv)]
    (reduce + (map triplet-score (take tenframes (partitionv-all 3 1 ballv))))))


;;; Note: a 10 can only occur in the first half of a frame.  The second half would be a :spare.

;;; h is the half-frames budget (counting down from 20), c is the count of triplets to score
;;; slightly faster than the original ten-frames
(defn ten-frames5 [bv]
  (peek (reduce (fn [[h c] b]
                  (cond (zero? h) (reduced [c])
                        (nil? b) (throw (ex-info "Incomplete sequence of bowling balls"
                                                 {:balls (seq bv)}))
                        (= b 10) [(dec (dec h)) (inc c)]
                        :else [(dec h) (inc c)]))
                [20 0]
                bv)))
            

(defn ballv [game]
  (into [] (comp (remove #{\space}) (map ch->ball)) game))

(defn score5 [game]
  (let [bv (ballv game)
        tenframes (ten-frames5 bv)]
    (reduce + (map triplet-score (take tenframes (partitionv-all 3 1 bv))))))




(defn make-frames [bv]
  (reduce-kv (fn [fv i b]
               (let [fr (peek fv)]
                 (if (empty? fr)
                   (if (= b 10) ;strike
                     (conj (pop fv) [10 (nth bv (inc i) nil) (nth bv (+ i 2) nil)] [])
                     (conj (pop fv) [b]))
                   (if (= b :spare)
                     (conj (pop fv) [10 (nth bv (inc i) nil)] [])
                     (conj (pop fv) (conj fr b) [])))))
             [[]]
             bv))

;;; score6 has to take 10 frames, make-frames might give too many
(defn score6 [game]
  (let [bv (ballv game)]
    (reduce (fn [s f]
              (+ s (reduce + f)))
            0
            (take 10 (make-frames bv)))))




(defn score-co [rolls]
  (let [rolls (seq rolls)]
    (cond
      (empty? rolls) 0
      (= "X" (first rolls)) (+ 10 (score (drop 1 rolls)) (score (drop 2 rolls)))
      (= "/" (first rolls)) (+ 10 (score (drop 2 rolls)) (first (drop 2 rolls)))
      :else (+ (first rolls) (score (drop 1 rolls))))))   



;;; SEM:  maybe a good idea to do the representation of exactly the ball rolls and not
;;; keep special encoding for spares.  But my :spare is slightly faster than checking for 10
;;; every frame.

(defn gamestr->rolls [game]
  (let [bv (ballv game)]
    (reduce-kv (fn [rs i b]
                 (conj rs
                       (if (= b :spare)
                         (- 10 (bv (dec i)))
                         b)))
               []
               bv)))


;;; found this one on the internet.  Different game rep so we had to adjust that.
;;; https://softnoise.wordpress.com/2010/02/07/the-bowling-kata-in-clojure/

;;; about half speed of my score but not bad.
(defn score-fra [game]
  (let [rolls (gamestr->rolls game)]
    (loop [rolls (seq rolls) frame 1 score 0]
      (if (> frame 10)
        score
        (let [s12 (reduce + (take 2 rolls))
              fscore (if (>= s12 10) (+ s12 (nth rolls 2)) s12)]
          (recur (drop (if (= 10 (first rolls)) 1 2) rolls)
                 (inc frame)
                 (long (+ score fscore))))))))


;;; SEM variations below

;;; Better conversion of game string.
;;; returns all rolls as numbers, no special spare marker.

(defn rollvec [game]
  (reduce (fn [r ch]
            (let [x (- (long ch) (long \0))]
              (case x
                40 (conj r 10)
                -1 (conj r (- 10 (peek r)))
                -3 (conj r 0)
                (conj r x))))
          []
          (str/replace game " " "")))

(defn score-fra3 [game]
    (loop [rolls (seq (rollvec game)) frame 1 score 0]
      (if (> frame 10)
        score
        (let [s12 (reduce + (take 2 rolls))
              fscore (if (>= s12 10) (+ s12 (nth rolls 2)) s12)]
          (recur (drop (if (= 10 (first rolls)) 1 2) rolls)
                 (inc frame)
                 (long (+ score fscore)))))))

