(ns miner.bowling
  (:require [clojure.string :as str])
  (:require [clojure.test :refer [is are deftest]]))


;;; Years later, I discovered that Uncle Bob and Stuart Halloway worked this problem.
;;; https://github.com/stuarthalloway/clojure-bowling

;;; My final score is much faster and simpler than my previous efforts.  I spent too much
;;; effort trying to get the triplet style scoring to work as in score1 and friends.

;;; There's no particular advantage to being lazy for a small program like this so I think
;;; it makes sense to be eager -- using reduce-kv.


;;; SEE ALSO my transmuters project for a variation (but not really better).

;; https://codingdojo.org/kata/Bowling/

;; game is a string, encoding balls rolled
;; X for strike
;; / for spare
;; - for a miss or gutter ball
;; 1-9 for that many pins
;; spaces are ignored

;; If last frame is a strike or spare, the extra balls accrue only to that frame.



;;; Main idea: use a vector to hold all the balls rolled so it is convenient to access the
;;; next two balls for scoring spares and strikes.  Using reduce-kv gives us the index i of
;;; the current ball b.  (That's faster than using map-indexed.)  Keep frame count `fc`,
;;; counting down from 20 half-frames to 0.  When fc is even, the ball is the first of a
;;; frame.  We score the frame only after the second ball, except for a strike which counts
;;; as a full frame (fc-2).  Counting down is slightly faster than counting up.  The
;;; odd/even scheme eliminates the need for a frame marker (nil in some of my other
;;; implementations) or keeping a vector of frames which is a bit slower than doing the
;;; running total score.
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


;;; new idea -- encode strike as 10 nil so frame count is synced and you don't have to count
;;; it manually.

;;; simpler and a little bit faster than score on some test runs
;;; probably better set up for input error checking

(defn score7 [game]
  (let [bv (reduce (fn [r c]
                     (let [x (- (long c) (long \0))
                           b (case x 40 10 -3 0 x)]
                       (if (= b 10)
                         (conj (conj r 10) nil)
                         (conj r b))))
                   []
                   (str/replace game " " ""))]
    ;;(println "score7 bv " bv)
    ;; note X 40, \ -1, - -3.  The test for a spare mark is neg?
    (reduce-kv (fn [sc i b]
                 (if (even? i)
                   ;; first ball of frame
                   (if (= b 10) ;strike
                     ;; skip nil padding
                     (let [b1 (bv (+ i 2))
                           b2 (bv (if (= b1 10) (+ i 4) (+ i 3)))]
                       (if (neg? b2) (+ sc 20) (+ sc 10 b1 b2)))
                     ;; don't score first ball, we will account for it on second ball
                     sc)
                   ;; second ball of frame, recover the previous ball if necessary
                   (cond (nil? b) sc
                         (neg? b) (+ sc 10 (bv (inc i)))
                         :else (+ sc (bv (dec i)) b))))
               0
               (subvec bv 0 20))))


;;; slower than veriscore

(defn score7v [game]
  ;; note x maps X 40, \ -1, - -3.  The test for a spare mark is neg?
  (let [bv (reduce (fn [r c]
                     (let [i (count r)
                           x (- (long c) (long \0))
                           b (case x 40 10 -3 0 (-1 0 1 2 3 4 5 6 7 8 9) x
                                   (throw (ex-info (str "Bad input " c)
                                                   {:bad-game game :bad-char c})))]
                       ;; odd, second ball
                       (if (odd? i)
                         (if (= b 10)
                           (throw (ex-info (str "Illegal strike in " game)
                                           {:bad-game game :index i}))
                           (when-not (neg? b)
                             (when (>= (+ (peek r) b) 10)
                               (throw (ex-info (str "Bad frame [" (peek r) b "] in game " game)
                                               {:bad-game game
                                                :bad-frame  [(peek r) b]})))))
                         ;; even, first ball
                         (when (neg? b)
                           (throw (ex-info (str "Illegal spare in " game)
                                           {:bad-game game :index i}))))

                       (if (= b 10)
                         (conj (conj r 10) nil)
                         (conj r b))))
                   []
                   (str/replace game " " ""))
        bcnt (count bv)]
    
    ;; error checking length
    ;; strikes at end need to account for nil padding
    (let [xcnt (cond (< bcnt 20) 20
                     (> bcnt 24) 24
                     :else (let [strike10? (= (bv 18) 10)
                                 spare10? (when-not strike10? (neg? (bv 19)))]
                             (cond (not (or strike10? spare10?)) 20
                                   (and strike10? (= (nth bv 20 nil) 10)
                                        (= (nth bv 22 nil) 10)) 24
                                   (and strike10? (= (nth bv 20 nil) 10)) 23
                                   strike10? 22
                                   (and spare10? (= (nth bv 20 nil) 10)) 22
                                   spare10? 21
                                   :else 20)))]
      (cond
       (< bcnt xcnt) (throw (ex-info (str "Insufficient balls in " game) {:bad-game game}))
       (> bcnt xcnt) (throw (ex-info (str "Too many balls in " game) {:bad-game game}))))
    
    (reduce-kv (fn [sc i b]
                 (if (even? i)
                   ;; first ball of frame
                   (if (= b 10) ;strike
                     ;; skip nil padding
                     (let [b1 (bv (+ i 2))
                           b2 (bv (if (= b1 10) (+ i 4) (+ i 3)))]
                       (if (neg? b2) (+ sc 20) (+ sc 10 b1 b2)))
                     ;; don't score first ball, we will account for it on second ball
                     sc)
                   ;; second ball of frame, recover the previous ball if necessary
                   (cond (nil? b) sc
                         (neg? b) (+ sc 10 (bv (inc i)))
                         :else (+ sc (bv (dec i)) b))))
               0
               (subvec bv 0 20))))



;;; Use logic of throw-on-invalid but return the vector of balls as used by score7, then do
;;; sum as in score7.  Note validation is all up front


(defn throw-on-invalid-game [game]
  ;; note x maps X 40, \ -1, - -3.  The test for a spare mark is neg?
  ;; 0 is not a valid char.  Use - for gutter ball.
  (if-not (string? game)
    (throw (ex-info "Invalid bowling game" {:bad-game game}))
    (let [game (str/replace game " " "")
          r (peek (reduce
                   (fn [[p r] c]
                     (let [x (- (long c) (long \0))
                           b (case x 40 10 -3 0 (-1 1 2 3 4 5 6 7 8 9) x
                                   (throw (ex-info (str "Bad input " c " in game " game)
                                                   {:bad-game game :bad-char c})))]

                       ;; r "remaining" is neg for extra balls, inc back towards zero
                       ;; -2 for extra strike, -1 for extra spare
                       (cond
                        (and (= r 2) (= b 10)) [nil -2]
                        (and (= r 1) (neg? b)) [nil -1]
                        (pos? r) (if (even? r)
                                   ;; first ball
                                   (if (= b 10)
                                     [nil (- r 2)]
                                     (if (neg? b)
                                       (throw (ex-info (str "Illegal spare in " game)
                                                       {:bad-game game :index r}))
                                       [b (dec r)]))
                                   ;; second ball
                                   (if (= b 10)
                                     (throw (ex-info (str "Illegal strike in " game)
                                                     {:bad-game game :index r}))

                                     (if (>= (+ p b) 10)
                                       (throw (ex-info (str "Bad frame [" p b "] in game " game)
                                                       {:bad-game game
                                                        :bad-frame  [p b]}))
                                       [nil (dec r)])))
                        (zero? r) (throw (ex-info (str "Too many balls in " game)
                                                  {:bad-game game}))
                        (neg? r) (cond (nil? p) [(when (not= b 10) b) (inc r)]
                                       (and (even? r) (neg? b))
                                           (throw (ex-info (str "Illegal spare in " game)
                                                           {:bad-game game :index r}))
                                       (>= (+ p b) 10)
                                           (throw (ex-info (str "Bad frame [" p b "] in game " 
                                                                game)
                                                           {:bad-game game
                                                            :bad-frame  [p b]}))
                                       :else [b (inc r)]))))
                   [nil 20]
                   game))]
      (when-not (zero? r)
        (throw (ex-info (str "Insufficient balls in " game) {:bad-game game}))))))

;;; with nil padding strikes for count < 20, but extra balls do not pad
(defn vvscore [game]
  ;; note x maps X 40, \ -1, - -3.  The test for a spare mark is neg?
  ;; 0 is not a valid char.  Use - for gutter ball.
  (if-not (string? game)
    (throw (ex-info "Invalid bowling game" {:bad-game game}))
    (let [game (str/replace game " " "")
          bv (reduce
              (fn [rv c]
                (let [x (- (long c) (long \0))
                      b (case x 40 10 -3 0 (-1 1 2 3 4 5 6 7 8 9) x
                              (throw (ex-info (str "Bad input " c " in game " game)
                                              {:bad-game game :bad-char c})))
                      cnt (count rv)]
                  (cond
                   (even? cnt)
                       ;; first ball of frame
                       (if (neg? b)
                         (throw (ex-info (str "Illegal spare in " game)
                                         {:bad-game game :index cnt}))
                         (if (and (= b 10) (< cnt 20))
                           ;; nil pad after strikes, but not for "extra" balls
                           (conj (conj rv 10) nil)
                           (conj rv b)))
                   
                   ;; odd cnt, second ball of frame
                   :else
                       (if (and (= b 10) (< cnt 20))
                         (throw (ex-info (str "Illegal strike in " game)
                                         {:bad-game game :index cnt}))
                         (let [p (peek rv)]
                           (if (and (= b 10) (>= cnt 20))
                             (conj rv b)
                             (if (>= (+ p b) 10)
                               (throw (ex-info (str "Bad frame [" p b "] in game " game)
                                               {:bad-game game
                                                :bad-frame  [p b]}))
                               (conj rv b))))))))

              []
              game)
          cnt (count bv)
          exp-cnt (cond (< cnt 20) 20
                        (= (bv 18) 10) 22
                        (neg? (bv 19)) 21
                        :else 20)]
      ;; check expected ball count
      (when (not= cnt exp-cnt)
        (if (< cnt exp-cnt)
          (throw (ex-info (str "Insufficient balls in " game) {:bad-game game}))
          (throw (ex-info (str "Too many balls in " game) {:bad-game game}))))
      (reduce-kv (fn [sc i b]
                   (if (even? i)
                     ;; first ball of frame
                     (if (= b 10) ;strike
                       ;; skip nil padding
                       (let [b1 (bv (+ i 2))
                             b2 (or (bv (+ i 3)) (bv (+ i 4)))]
                         (if (neg? b2) (+ sc 20) (+ sc 10 b1 b2)))
                       ;; don't score first ball, we will account for it on second ball
                       sc)
                     ;; second ball of frame, recover the previous ball if necessary
                     (cond (nil? b) sc
                           (neg? b) (+ sc 10 (bv (inc i)))
                           :else (+ sc (bv (dec i)) b))))
                 0
                 (subvec bv 0 20)))))






(defn valid-game? [game]
  (try (throw-on-invalid-game game)
       true
       (catch Throwable e #_ (println (ex-message e)) false)))


(defn vscore [game]
  (throw-on-invalid-game game)
  (score game))





;;; with built-in verificaton of input
;;; lots of extra code but not much of a performance hit
(defn veriscore [game]
  (let [bv (mapv (fn [c] (let [x (- (long c) (long \0))]
                           (case x 40 10 -3 0 (-1 0 1 2 3 4 5 6 7 8 9) x
                                 (throw (ex-info (str "Bad input " c) {:bad-game game
                                                                       :bad-char c})))))
                 (str/replace game " " ""))
        bcnt (count bv)]
    ;; note X 40, \ -1, - -3.  The test for a spare mark is neg?
    (let [[fc sc] (reduce-kv (fn [[fc sc] i b]
                               (if (zero? fc)
                                 (if (or (and (= (bv (dec i)) 10) (= (+ i 2) bcnt))
                                         (and (neg? (bv (dec i))) (= (inc i) bcnt)))
                                   (reduced [0 sc])
                                   (throw (ex-info (str "Extra rolls in " game)
                                                   {:bad-game game :fc fc :i i :b b})))
                                 (if (even? fc)
                                   ;; first ball of frame
                                   (if (= b 10) ;strike
                                     (if-not (> bcnt (+ i 2))
                                       (throw (ex-info (str "Insufficient balls in " game)
                                                       {:bad-game game
                                                        :b b
                                                        :i i}))
                                       (if (neg? (bv (inc i)))
                                         (throw (ex-info (str "Illegal spare in " game)
                                                         {:bad-game game
                                                          :b b
                                                          :i i}))
                                         (let [b2 (bv (+ i 2))]
                                           [(- fc 2)
                                            (if (neg? b2) (+ sc 20) (+ sc 10 (bv (inc i)) b2))])))
                                     ;; don't score first ball,
                                     ;; we will account for it on second ball
                                     (if (neg? b)
                                       (throw (ex-info (str "Illegal spare in " game)
                                                       {:bad-game game
                                                        :b b
                                                        :i i}))
                                       [(dec fc) sc]))
                                   ;; second ball of frame, recover the previous ball if necessary
                                   [(dec fc)
                                    (if (neg? b)
                                      (if-not (> bcnt (inc i))
                                        (throw (ex-info (str "Insufficient balls in " game)
                                                        {:bad-game game
                                                         :b b
                                                         :i i}))
                                        (+ sc 10 (bv (inc i))))
                                      (let [frame (+ (bv (dec i)) b)]
                                        (if (< frame 10)
                                          (+ sc frame)
                                          (throw (ex-info (str "Bad frame " (bv (dec i)) b
                                                               " in game " game)
                                                          {:bad-game game
                                                           :bad-frame  [(bv (dec i)) b]})))))])))
                             ;; init at 20 half-frames, and zero score
                             [20 0]
                             bv)]
      (if (zero? fc)
        sc
        (throw (ex-info (str "Frame error, " fc " remaining")
                        {:bad-game game :bad-frame-count fc}))))))



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
   (assert (= (score "12 12 12 12 12 12 12 12 12 -/X") 47))
   (assert (= (score "XX 3/ 4/ X 54 7/ X X X 3/")  198))
   ;;; illegal (assert (= (score "XX 3/ 4/ X 54 7/ X X X 37")  198))
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



(deftest scoring
  (is 223 (score "35 6/ 7/ X 45 X X X XXXX"))
  (is 30 (score "11 11 11 11 11 11 11 11 X 11"))
  (is 30 (score "11 11 11 11 11 11 11 11 11 X 11"))
  (is 300 (score "XXXXXXXXXXXX"))
  (is 90 (score "9-9-9-9-9-9-9-9-9-9-"))
  (is 150 (score "5/5/5/5/5/5/5/5/5/5/5"))
  (is 47 (score "12 12 12 12 12 12 12 12 12 0/X"))
  (is 198 (score "XX 3/ 4/ X 54 7/ X X X 3/"))
  )

(defn validating-score [score]
  (is (thrown? clojure.lang.ExceptionInfo (score "11 11 11 /1 11 11 11 11 11 X 11")))
  (is (thrown? clojure.lang.ExceptionInfo (score "11 56 11 11 11 11 11 11 X 11")))
  (is (thrown? clojure.lang.ExceptionInfo (score "XX 3/ 4/ X 54 7/ X X 37")))
  (is (thrown? clojure.lang.ExceptionInfo (score "XXX")))
  true)

(deftest validating
  (validating-score score7v))

(deftest validating2
  (validating-score veriscore))
