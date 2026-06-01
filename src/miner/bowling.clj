(ns miner.bowling)

;;; Years later, I discovered that Uncle Bob and Stuart Halloway worked this problem.
;;; https://github.com/stuarthalloway/clojure-bowling

;;; My final score is much faster and simpler than my previous efforts.  I spent too much
;;; effort trying to get the triplet style scoring to work as in score1 and friends.

;;; SEE ALSO my transmuters project for a variation (but not really better).

;; http://codingdojo.org/cgi-bin/index.pl?KataBowling
;; https://www.javacodegeeks.com/2016/05/bowling-kata-clojure-f-scala.html

;; game is a string, encoding balls rolled
;; X for strike
;; / for spare
;; - for a miss or gutter ball
;; 1-9 for that many pins
;; spaces are ignored

;; If last frame is a strike or spare, the extra balls only accrue to that frame.

;; A "ball" re-encodes the character as 0 to 10 or :spare.  This is for readability and
;; more convenient scoring.

(defn ch->ball [ch]
  (case ch
    \X 10
    \/ :spare
    \- 0
    (- (long ch) (long \0))))



;;; Much faster
;;; using nil as the new frame marker, otherwise the (peek fv) is the first ball of the frame.
;;; basically, goes to 11 frames, but always pop last nil before adding up. Only counting
;;; 10 frames.

;;; was bugby when I tried to put pop into reduced expr because sometimes the last ball is
;;; just enough to finish.  In which case, it doesn't terminate early on the count fv.  The
;;; outside pop is necessary to clear the placeholder nil as done correcly in score.  The
;;; check on the frame count also protects access to later balls.  Assuming legal games.


(defn score [game]
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



(defn score13 [game]
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

;; slightly faster to avoid conj/3
(defn conj3 [coll a b]
  (conj (conj coll a) b))


;; transduce with map-indexed not faster, but not bad
(defn score15 [game]
  (let [bv (into [] (comp (remove #(= % \space)) (map ch->ball)) game)]
    (transduce (map-indexed vector)
               (fn ([fv] (reduce + 0 (pop fv)))
                 ([fv [i b]]
                  (if (= (count fv) 11)
                    (reduced fv)
                    (let [fr (peek fv)
                          pfv (pop fv)]
                      (if (nil? fr)
                        (if (= b 10) ;strike
                          (let [b2 (bv (+ i 2))]
                            (if (= b2 :spare)
                              (conj3 pfv 20 nil)
                              (conj3 pfv (+ 10 (bv (inc i)) b2) nil)))
                          (conj pfv b))
                        (if (= b :spare)
                          (conj3 pfv (+ 10 (bv (inc i))) nil)
                          (conj3 pfv (+ fr b) nil)))))))
               [nil]
               bv)))




;;; new idea, frame marker is always neg of next ball index, -1 means current ball zero, -2
;;; for ball 1, etc.




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

(defn partest []
  (doseq [g games]
    (let [balls (ball-seq g)
          tenfr (ten-frames balls)
          pa31 (partition-all 3 1  balls)
          p31 (partition 3 1 [nil nil] balls)
          sc (score g)
          sc3 (score g)]
      (when (not= sc sc3)
        (println "  " sc sc3 "  " tenfr g)
        (println "p-all" pa31)
        (println "p    " p31)))))



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



;;; SEM:  probably a good idea to do the representation of exactly the ball rolls and not
;;; keep special encoding for spares.

(defn gamestr->rolls [game]
  (let [bv (ballv game)]
    (reduce-kv (fn [rs i b]
                 (conj rs
                       (if (= b :spare)
                         (- 10 (bv (dec i)))
                         b)))
               []
               bv)))

;;; all numbers, no :spare, / translated to appropriate roll
(defn rollvec [game]
  (reduce (fn [r ch]
            (case ch
              \space r
              \X (conj r 10)
              \/ (conj r (- 10 (peek r)))
              \- (conj r 0)
              (conj r (- (long ch) (long \0)))))
          []
          game))

;;; found this one on the internet.  Different game rep so we had to adjust that.
;;; https://softnoise.wordpress.com/2010/02/07/the-bowling-kata-in-clojure/

;;; about half speed of my score but not bad.  Maybe could do better conversion of gamestr
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
(defn score-fra2 [game]
    (loop [rolls (seq (rollvec game)) frame 1 score 0]
      (if (> frame 10)
        score
        (let [s12 (reduce + (take 2 rolls))
              fscore (if (>= s12 10) (+ s12 (nth rolls 2)) s12)]
          (recur (drop (if (= 10 (first rolls)) 1 2) rolls)
                 (inc frame)
                 (long (+ score fscore)))))))


;;; pretty good but not as fast as my score
(defn score-fra3 [game]
  (let [bv (rollvec game)]
    (reduce + 0
            (pop
             (reduce-kv (fn [fv i b]
                          (if (= (count fv) 11)
                            (reduced fv)
                            (let [fr (peek fv)]
                              (if (nil? fr)
                                (if (= b 10) ;strike
                                  (conj (pop fv) (+ 10 (bv (inc i)) (bv (+ i 2))) nil)
                                  (conj (pop fv) b))
                                (let [b2 (+ fr b)]
                                  (if (= b2 10) ;spare
                                    (conj (pop fv) (+ 10 (bv (inc i))) nil)
                                    (conj (pop fv) b2 nil)))))))
                        [nil]
                        bv)))))

;;; NOT CHANGED YET
(defn score-fra4 [game]
  (let [bv (rollvec game)]
    (reduce + 0
            (pop
             (reduce-kv (fn [fv i b]
                          (if (= (count fv) 11)
                            (reduced fv)
                            (let [fr (peek fv)]
                              (if (nil? fr)
                                (if (= b 10) ;strike
                                  (conj (pop fv) (+ 10 (bv (inc i)) (bv (+ i 2))) nil)
                                  (conj (pop fv) b))
                                (let [b2 (+ fr b)]
                                  (if (= b2 10) ;spare
                                    (conj (pop fv) (+ 10 (bv (inc i))) nil)
                                    (conj (pop fv) b2 nil)))))))
                        [nil]
                        bv)))))
