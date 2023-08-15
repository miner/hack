(ns miner.pedro
  (:require (clojure [string :as str])))

;;; See pedro.txt for more info

;;;----------------------------------------------------------------------
;;; was pedro/util.clj


;; original:
;; http://groups.google.com/group/clojure/browse_thread/thread/234ac3ff0a4b6b80?pli=1
;; but slightly changed for Clojure updates since 1.0

(defn unmangle
"Given the name of a class that implements a Clojure function, returns the function's name in Clojure. Note: If the true Clojure function name
  contains any underscores (a rare occurrence), the unmangled name will
  contain hyphens at those locations instead."
  [class-name]
  (.replace
   (clojure.string/replace class-name #"^(.+)\$([^@]+)(|@.+)$" "$1/$2")
   \_ \-))

;; only appropriate for debugging
(defmacro current-fn-name []
  "Returns a string, the name of the current Clojure function; very expensive -- use only for debugging"
  `(unmangle (.getClassName ^StackTraceElement (first (.getStackTrace (Throwable.))))))

(defmacro unfinished []
  `(println "Warning: unfinished fn " (current-fn-name)))

(defmacro not-implemented []
  `(throw (ex-info "Unfinished" {:frame (current-fn-name)})))


;; Dynamic var (as in 1.3)
;; Not the same as clojure.contrib.def version
(defmacro defvar
  "Defines a dynamic var with an optional intializer and doc string"
  ([name]
     (list `def (with-meta name (assoc (meta name) :dynamic true))))
  ([name init]
     (list `def (with-meta name (assoc (meta name) :dynamic true)) init))
  ([name doc-string init]
     (list `def (with-meta name (assoc (meta name) :doc doc-string :dynamic true)) init)))


;; expects numbers as result of keyfn, but special cases nil arg and nil result, treating
;; them as MIN_VALUE for convenience
(defn index-of-max [keyfn coll]
  (let [fk #(or (and % (keyfn %)) Long/MIN_VALUE)]
    (when-let [s (seq coll)]
      (loop [mi 0 mv (fk (first s)) i 0 cs (next s)]
        (if-let [cs (seq cs)]
          (let [v (fk (first cs))]
            (let [i (inc i)]
              (if (> mv v)
                (recur mi mv i (next cs))
                (recur i v i (next cs)))))
          mi)))))


(defn index-of [val coll]
  (first (keep-indexed #(when (= val %2) %1) coll)))

;;;----------------------------------------------------------------------


; card is a map [:rank :suit]


;; suit-keyword is one of [:spades :clubs :diamonds :hearts]



;; ace is high (140), not low (10) -- there is no 0 or 10 rank

;; card value rank + 100 for lead suit or + 1000 for trump
;; (mod val 100) == rank

;; ranks: A 140, K 130, Q 120, J 110, ten 100, nine 90, 80, 70, 60, pedro 50, lowpedro 49, 40, 30, 20


;; bids are rendered as vector [num suit], but the suit is considered private to the

(defvar *debug* false)

(when *debug*
  (println "start"))

(def ace 140)
(def king 130)
(def queen 120)
(def jack 110)

(def pedro 50)
(def low-pedro 49)

(def rank-max+ (inc ace))
(def rank-min 20)

(def winning-score 91)

(declare trump?)

(defn suit-char [card trump]
  (if (trump? card trump)
    (str/upper-case (name (:suit card)))
    (name (:suit card))))

(defn cardstr
  ([card] (cardstr card nil))
  ([card trump]
     (when-let [r (:rank card)]
       (if (zero? r)
         "joker"
         ;; note that case requires literals, not symbolic refs
         (str (case (long r)
                140 "A"
                130 "K"
                120 "Q"
                110 "J"
                49 "P"
                (quot r 10))
              (suit-char card trump))))))

(defn create-card [rank suit]
  (assoc {} :rank rank :suit suit))

;; used only when there are no cards left to deal
(def joker (create-card 0 :x))

(defn parse-card [s]
  {:pre [(or (= s "joker") (< 1 (count s) 4))]}
  (if (= s "joker")
    joker
  (let [last (dec (count s))
	suit (keyword (str/lower-case (subs s last)))
	rank-str (subs s 0 last)
	rank    (case rank-str
			  ("A" "a") ace
			  ("K" "k") king
			  ("Q" "q") queen
			  ("J" "j") jack
                          ("P" "p") low-pedro
                          (* 10 (load-string rank-str)))]
    (create-card rank suit))))
	  
(def ranks (vec (range rank-min rank-max+ 10)))

(def trump-ranks (vec (sort-by identity > (conj ranks low-pedro))))

(def suits [:s :c :d :h])
;; (def suits [:spades :clubs :diamonds :hearts])
(def same-color-suit {:s :c :c :s :d :h :h :d})

(def ordered-cards (vec (for [s suits r ranks] (create-card r s))))


(defn low-pedro? [c trump]
  (and (= (:rank c) pedro)
       (= (:suit c) (same-color-suit trump))))

(defn trump-suit? [c trump]
  (= (:suit c) trump))

(defn trump? [c trump]
  (or (trump-suit? c trump)
      (low-pedro? c trump)))

(defn high-pedro? [c trump]
  (and (= (:rank c) pedro)
       (= (:suit c) trump)))

(defn pedro? [c trump]
  (or (high-pedro? c trump) (low-pedro? c trump)))

(defn jack-trump? [c trump]
  (and (= (:rank c) jack)
       (= (:suit c) trump)))
  
(defn ace-trump? [c trump]
  (and (= (:rank c) ace)
       (= (:suit c) trump)))
  
(defn ten-trump? [c trump]
  (and (= (:rank c) 100)
       (= (:suit c) trump)))
  
(defn two-trump? [c trump]
  (and (= (:rank c 20))
       (= (:suit c) trump)))


(defn joker? [c]
  (and c (zero? (:rank c))))
  
(defn card-value
  ([c trump] (card-value c trump trump))
  ([c trump lead]
     (cond (nil? c) 0
           (joker? c) 0
           (low-pedro? c trump) 1049
           :else (+ (:rank c) (condp = (:suit c) trump 1000 lead 200 0)))))

(defn trump-rank [c trump]
  (max (- (card-value c trump) 1000) 0))

(def non-point-ranks [30 40 60 70 80 90])

(def pedro-ranks [49 50])


;; The high trump takes these points, but not for the Two
(defn winner-points [c trump]
  (if (and c (trump? c trump))
    (condp = (:rank c)
      ace 1
      100 1
      jack 3
      pedro 5
      low-pedro 5
      0)
    0))

;; the Two wins itself
(defn two-points [c trump]
  (if (and c (and (trump? c trump) (= (:rank c) 20)))
    1
    0))



(when *debug* (println "second"))

;; changing players to be index 0..3

(def players (vec (range 4)))
(def partner [2 3 0 1])
(def next-player [1 2 3 0])
(def previous-player [3 0 1 2])
(def team [0 1 0 1])

(defn player-order [starting-player]
  (take 4 (iterate next-player starting-player)))


(defn nvec [n coll]
  (vec (repeat n coll)))


(defn new-game []
  {:deck (shuffle ordered-cards)
   :hands (nvec 4 [])
   :kept (nvec 4 -1)
   :burn (nvec 4 0)
   :leader []
   :tricks []
   :played []
   :trump nil
   :dealer 0
   :bids (nvec 4 [0 nil])
   :bidder 0
   :points (nvec 4 0)
   :score [0 0]})


(defn leader [game]
  (or (peek (:leader game))
      (next-player (:dealer game))))

(defn trickstr [trick trump leader winner]
  (with-out-str
    (print "[")
    (dotimes [n (count trick)]
      (when-not (zero? n) (print " "))
      (when (== n leader) (print "^"))
      (print (cardstr (trick n) trump))
      (when (== n winner) (print "+")) )
    (print "]")))
      
(defn prg [game]
  (println)
  (let [trump (:trump game)
        cstr #(cardstr % trump)]
  (dotimes [p 4]
    (print "hand" p (map cstr (get-in game [:hands p])))
    (print "  kept:" (get-in game [:kept p]))
    (when (or (= (get (:leader game) 0) p) (= (get-in game [:kept p]) -1))
      (print "  bid " (get-in game [:bids p])))
    (when (pos? (get-in game [:burn p]))
      (print "  burn:" (get-in game [:burn p])))
    (println))
  (println "dealer: " (:dealer game))
  (print "leads: " (leader game))
  (println ",   leader " (:leader game))
  (println "trump: " (:trump game))
  (println "deck: " (map cstr (:deck game)))
  (println "tricks: " (map-indexed (fn [i trk] (trickstr trk (:trump game) (get-in game [:leader i]) (get-in game [:leader (inc i)]))) (:tricks game)))
  (println "points: " (:points game))
  (println "score: " (:score game))))

(defn tap [obj fn-side-effect & print-args]
  (when (seq print-args) (apply println "\ndebug:" print-args))
  (fn-side-effect obj)
  obj)

 ;; (doseq [trick (:tricks game)]    (println (map cardstr trick))))
  
(defn peek-deck [game]
  (or (peek (:deck game)) joker))

(defn pop-deck [game]
  (if (empty? (:deck game))
    game
    (update-in game [:deck] pop)))

(defn deal1 [game p]
  (let [card (peek-deck game)]
    (update-in (pop-deck game) [:hands p] conj card)))

(defn dealn [game player n]
  (reduce (fn [g p] (deal1 g p)) game (repeat n player)))

(defn deal3 [game player]
  (dealn game player 3))

(defn deal [game]
  (reduce (fn [g p] (deal3 g p)) game (take 12 (cycle players))))


(defn update-game [game update-player-fn]
  (reduce #(update-player-fn %1 %2) game (player-order (leader game))))


;; needs work
;; -1 is a pass

(def pass-bid -1)
(def max-bid 16)
(def min-bid 6)
(def tricks 6)

;; indices for bid vector
(def ibid 0)
(def isuit 1)

;; bid is [num suit]

(when *debug* (println "third"))

(defn my-bid-suit [hand trump]
  (let [has-low-pedro (first (filter #(low-pedro? % trump) hand))
        ranks (set (map :rank (filter #(trump? % trump) hand)))
        has-ace (ranks ace)
        has-king (ranks king)
        has-queen (ranks queen)
        has-jack (ranks jack)
        has-pedro (ranks pedro)
        both-pedros (and has-low-pedro has-pedro)
        one-pedro (and (not both-pedros) (or has-low-pedro has-pedro))
        has-ten (ranks 100)
        has-two (ranks 20)
        my-bid    (cond (and has-ace has-king has-two one-pedro) 9
                        (and has-ace (or has-king has-queen has-jack)
                             (>= (count ranks) 4)) 8
                             (and has-ace (or has-jack has-ten has-two)) 7
                             has-ace 6
                             :else pass-bid)]
    [my-bid trump]))

(defn my-dealer-bid [hand p]
  (let [aces (filter #(= (:rank %) ace) hand)
        grouped-cards (group-by :suit (sort-by :rank hand))
        st (cond (zero? (count aces)) (first (apply max-key (comp count val) grouped-cards))
                 (= 1 (count aces)) (:suit (first aces))
                 :else (first (reduce #(if (>= (count (second %1)) (count (second %2))) %1 %2)
                                      grouped-cards)))]

    [6 st]))



;; needs work
(defn my-bid [game p]
  (let [my-hand (get-in game [:hands p])
        bid (apply max-key first (map #(my-bid-suit my-hand %) suits))
        bid-num (first bid)
        partner-bid (get-in game [:bids (partner p) ibid])
        opp-bid (get-in game [:bids (next-player p) ibid])
        opp2-bid (get-in game [:bids (previous-player p) ibid])
        high-bid (first (apply max-key first (:bids game)))]
    (if (= (:dealer game) p)
      ;; dealer only needs to bid +1 highest, or min-bid
      (cond (> bid-num high-bid) [(max 6 (inc high-bid)) (bid isuit)]
            (< high-bid min-bid) (my-dealer-bid my-hand p)
            :else [pass-bid nil])
      (cond (and (> partner-bid min-bid) (= partner-bid high-bid)) [pass-bid nil]
            (> bid-num high-bid) bid
            :else [pass-bid nil]))))


(defn take-bid [game p]
  (assoc-in game [:bids p] (my-bid game p)))

(defn take-bids [game]
  (update-game game take-bid))


(defn mreduce [f val coll & more]
  (not-implemented))



(defn declare-trump [game]
  (let [high-bidder (index-of-max first (:bids game))]
    (-> game
        (assoc-in [:bidder] high-bidder)
        (update-in [:leader] conj high-bidder)
        (assoc :trump (get-in game [:bids high-bidder 1])))))

(defn filter-trump [trump hand]
  (filter #(trump? %1 trump) hand))


(defn drop-non-trump-player [game p]
  (let [trump (:trump game)
        kept-trump (filter-trump trump (get-in game [:hands p]))]
    (-> game
        (assoc-in [:hands p] kept-trump)
        (assoc-in [:kept p] (count kept-trump)))))

(defn drop-non-trump [game]
  (update-game game drop-non-trump-player))

(defn deal-new-cards-player [game p]
  (let [hand (get-in game [:hands p])]
    (dealn game p (- tricks (count hand) ))))

(defn deal-new-cards [game]
  (update-game game deal-new-cards-player))

(defn burn-extra-non-point-cards [game]
  (unfinished)
  game)


(defn show-card [game p]
  (not-implemented))

;; lots to do
(defn drop-card [game p card]
  (update-in game [:hands p] #(remove #{card} %)))

;;show-card

;;maybe should be inlined

(when *debug* (println "fourth"))



(defn sort-cards-trump [cards trump]
  ;; sort so that highest trump is first, lowest non-trump last
  (let [card-val #(- (card-value % trump trump))]
    (sort-by card-val cards)))

(defn all-played-cards [game]
  (remove nil? (apply concat (:tricks game))))

(defn sorted-played-cards [game]
  (sort-cards-trump (all-played-cards game) (:trump game)))

(defn discard [hand card]
  (remove #{card} hand))

(defn player-trumps [game p]
  (let [trump? #(trump? % (:trump game))
        rank #(trump-rank % (:trump game))]
    (apply sorted-set-by > (map rank (filter trump? (get-in game [:hands p]))))))

(defn played-trump [game]
  (let [trump? #(trump? % (:trump game))
        rank #(trump-rank % (:trump game))]
    (apply sorted-set-by > (map rank (filter trump? (apply concat (:tricks game)))))))

(defn played-trump-by [game p]
  (let [trump? #(trump? % (:trump game))
        rank #(trump-rank % (:trump game))]
    (apply sorted-set-by > (map rank (filter trump? (map #(get % p) (:tricks game)))))))

(defn highest-unplayed-trump [game]
  (let [played (played-trump game)]
    (first (drop-while played trump-ranks))))

(defn hand-trumps [hand trump]
  (let [trump? #(trump? % trump)
        rank #(trump-rank % trump)]
    (apply sorted-set-by > (map rank (filter trump? hand)))))

(defn highest-trump [hand trump]
  (let [c (first hand)]
    (when (trump? c trump)
      c)))

(defn medium-trump [hand trump]
  (loop [hand hand]
    (when-let [c (first hand)]
      (when (trump? c trump)
        (if (> 100 (:rank c) 5)
          c
          (recur (rest hand)))))))

(defn off-suit
  ([hand trump] (off-suit hand trump nil))
  ([hand trump led-suit]
  ;; try to be on led-suit if possible
  (let [non-trump (remove #(trump? % trump) hand)]
    (or (and led-suit (first (filter #(= (:suit %) led-suit) non-trump)))
        (first non-trump)))))
        

(defn high-trump [hand trump]
  (let [c (first hand)]
    (when (and (trump? c trump) (some #{(:rank c)} [ace king queen]))
      c)))




;;; SEM BUGGY FIXME
(defn high-points [hand trump]
  (let [points #(winner-points % trump)]
    (get hand (index-of-max points hand))))

(defn find-pedro [hand trump]
  (loop [cards hand]
    (when (seq cards)
      (if (pedro? (first cards) trump)
        (first cards)
        (recur (rest cards))))))


;; map of trump-rank to potential point card
(def point-ranking
  {20 100
   30  1
   40  2
   49 499
   50 500
   60  3
   70  4
   80  5
   90  6
   100 110
   110 300
   120 90
   130 95
   140 150})


(defn create-trump-card [rank trump]
  (if (== rank low-pedro)
    (create-card pedro (same-color-suit trump))
    (create-card rank trump)))



(defn find-low-trump [hand trump]
  (let [trump? #(trump? % trump)
        rank #(trump-rank % trump)
        my-trumps (map rank (filter trump? hand))]
    (when (seq my-trumps)
      (let [lowest-rank (apply min-key point-ranking my-trumps)]
          (create-trump-card lowest-rank trump)))))
  

(defn max-card [trump card others]
  (let [card-val #(card-value % trump)]
    (every? #(>= (card-val card) %) (map card-val others))))

(declare winner)

;; typically called with an incomplete trick, but same logic as winner
(defn winning [trick trump]
  (winner trick trump trump))



;; SEM FIXME -- still very crude
;; this takes some intelligence and must follow constraints

;; JUST ONE OF MANY BUGS: winning for partner should consider your cards as well since no one else can trump him;
;; don't play a higher card to beat your partner

;; doesn't play on suit for non-trump
(defn choose-card-old [game p]
  (let [trick-num (dec (count (:tricks game)))
        trump (:trump game)
        leader (peek (:leader game))
        played (played-trump game)
        my-hand (get-in game [:hands p])
        [c0 c1 c2 c3 :as trk] (peek (:tricks game))
        card-to-play    (cond (== p leader) (high-trump my-hand trump)
                              (and (== (partner p) leader) (== (winning trk trump) leader))
                              (if-let [pedro (find-pedro my-hand trump)]
                                pedro
                                (find-low-trump my-hand trump))
                              (max-card (:trump game) (high-trump my-hand trump) trk) (high-trump my-hand trump)
                              ;; UNIMPLEMENTED: oppenent winning, you're third, play to prevent pedro winning itself
                              (trump? (get trk leader) trump) (find-low-trump my-hand trump))]
    (or card-to-play
        (rand-nth my-hand))))



(defn choose-card-lead [game p]
  (let [trick-num (dec (count (:tricks game)))
        trump (:trump game)
        leader p
        my-hand (get-in game [:hands p])
        trumps (hand-trumps my-hand trump)
        my-high (highest-trump my-hand trump)
        my-medium (medium-trump my-hand trump)
        my-off-suit (off-suit my-hand trump)
        my-low-trump (find-low-trump my-hand trump)        
        highest (highest-unplayed-trump game)]
    (or (and highest my-high (= my-high highest) my-high)
        (and (zero? trick-num) my-high)
        (and (> (count trumps) (- 4 trick-num)) my-medium)
        my-off-suit)))


(defn choose-card-first-opponent [game p clead]
  (let [trick-num (dec (count (:tricks game)))
        trump (:trump game)
        led-trump (trump? clead trump)
        led-rank (trump-rank clead trump)
        leader (previous-player p)
        my-hand (get-in game [:hands p])
        trumps (hand-trumps my-hand trump)
        my-high (highest-trump my-hand trump)
        my-medium (medium-trump my-hand trump)
        my-off-suit (off-suit my-hand trump (:suit clead))
        my-two (when (contains? trumps 20) (create-card 20 trump))
        my-low-trump (or my-two (find-low-trump my-hand trump)) 
        highest (highest-unplayed-trump game)]
    (or (and my-high highest (= my-high highest) my-high)
        (and led-trump (> (trump-rank my-high trump) led-rank) my-high)
        (and led-trump my-low-trump)
        (and (not led-trump) (> (count trumps) (- 4 trick-num)) my-medium)
        (and (not led-trump) my-off-suit))))


;; SEM FIXME Bug don't count your own cards as higher since opps can't have them
;; look for opp points
(defn choose-card-partner [game p clead copp]
  (let [trick-num (dec (count (:tricks game)))
        trump (:trump game)
        led-trump (trump? clead trump)
        led-rank (trump-rank clead trump)
        opp-rank (trump-rank copp trump)
        leader (partner p)
        my-hand (get-in game [:hands p])
        trumps (hand-trumps my-hand trump)
        my-high-pedro (when (contains? trumps 50) (create-card 50 trump))
        my-low-pedro (when (contains? trumps 49) (create-card 49 trump))
        my-pedro (or my-low-pedro my-high-pedro)
        my-high (highest-trump my-hand trump)
        my-medium (medium-trump my-hand trump)
        my-off-suit (off-suit my-hand trump (:suit clead))
        my-two (when (contains? trumps 20) (create-card 20 trump))
        my-low-trump (or my-two (find-low-trump my-hand trump)) 
        highest (highest-unplayed-trump game)]
    (or (and led-trump highest (> led-rank opp-rank) (> led-rank highest) my-pedro)
        (and led-trump (>  (trump-rank my-high trump) led-rank) my-high)
        (and led-trump my-low-trump)
        (and (not led-trump) (> (count trumps) (- 4 trick-num)) my-medium)
        (and (not led-trump) my-off-suit))))



(defn better-than [trumps rnk]
  (first (drop-while #(<= % rnk) (rseq trumps))))
  
(defn choose-card-last [game p clead cmypart clpartner]
  (let [trick-num (dec (count (:tricks game)))
        trump (:trump game)
        my-hand (get-in game [:hands p])
        trumps (hand-trumps my-hand trump)
        led-trump (trump? clead trump)
        led-rank (trump-rank clead trump)
        leader (next-player p)
        trick (peek (:tricks game))
        pwinning (winning trick trump)
        wrank (trump-rank (trick pwinning) trump)
        my-winner-rank (better-than trumps wrank)
        my-partner-winning (= pwinning (partner p))
        my-high-pedro (when (contains? trumps 50) (create-card 50 trump))
        my-low-pedro (when (contains? trumps 49) (create-card 49 trump))
        my-pedro (or my-low-pedro my-high-pedro)
        my-medium (medium-trump my-hand trump)
        my-off-suit (off-suit my-hand trump (:suit clead))
        my-two (when (contains? trumps 20) (create-card 20 trump))
        my-low-trump (or my-two (find-low-trump my-hand trump))]
    (or (and my-partner-winning my-pedro)
        (and (not my-partner-winning)
             (> (reduce + (map #(winner-points % trump) [clead cmypart clpartner])) 1)
             my-winner-rank
             (create-card my-winner-rank trump))
        (and led-trump my-low-trump) 
        (and (not led-trump) (> (count trumps) (- 4 trick-num)) my-medium)
        (and (not led-trump) my-off-suit))))




(defn choose-card [game p]
  (let [trk (peek (:tricks game))
        leader (peek (:leader game))
        pos (count (remove nil? trk))]
    (or
     (case pos
      0 (choose-card-lead game p)
      1 (choose-card-first-opponent game p (trk leader))
      2 (choose-card-partner game p (trk leader) (trk (next-player leader)))
      3 (choose-card-last game p (trk leader) (trk (next-player leader)) (trk (partner leader))))
      ;; default to make up for buggy code above
     (let [hand (get-in game [:hands p])]
       ;; this should never happen
       (println "WARNING: picking random card for Player " p " in trick " trk)
       (rand-nth hand)))))

        
(defn trick-current-num [game]
  (dec (count (:tricks game))))

(defn current-trick [game]
  (peek (:tricks game)))

(defn play-card [game p]
  (let [my-card (choose-card game p)
        trick (trick-current-num game)]
    (-> game
        (update-in [:hands p] discard my-card)
        (assoc-in [:tricks trick p] my-card)
        ;;(tap prg)
        )))
    


(defn tally-two [game two-player]
  (if two-player
    (update-in game [:points two-player] inc)
    game))

(defn tally-points-winner [game points winner]
  (update-in game [:points winner] + points))

(defn winner [trick trump lead-suit]
  (index-of-max #(card-value % trump lead-suit) trick))
  
(defn tally-points [game]
  (let [trk (current-trick game)
        lead-suit (:suit (trk (leader game)))
        winningp (winner trk (:trump game) lead-suit)
        points (reduce + (map #(winner-points % (:trump game)) trk))
        two-player (index-of (create-card 20 (:trump game)) trk)]

    (-> game
        (update-in [:leader] conj winningp)
        (tally-two two-player)
        (tally-points-winner points winningp)
        )))



(defn new-trick []
  (nvec 4 nil))

(defn play-trick [game]
  (-> game
      (update-in [:tricks] conj (new-trick))
      (update-game play-card)
      (tally-points)
      ;;(tap prg "play-trick")
  ))

(def burn-order [30 40 60 70 80 90 queen king])
(def burn-set (set burn-order))


(defn burn-extra-trump-player [game p]
   (let [my-trump (filter #(trump? %1 (:trump game)) (get-in game [:hands p]))
         burn (- (count my-trump) tricks)]
     (if (pos? burn)
       (let [sorted-trump (sort-by :rank (filter #(burn-set (:rank %)) my-trump))
             victims (take burn sorted-trump)
             survivors (remove (apply hash-set victims) my-trump)]
         ;; debug
         (println "burning " victims "in" my-trump)
         (-> game
             (assoc-in [:burn p] (count victims))
             (assoc-in [:hands p] survivors)))
       game)))


(defn burn-extra-trump [game]
  (reduce #(burn-extra-trump-player %1 %2) game (player-order (:dealer game))))



(defn sort-cards-player [game p]
  (update-in game [:hands p] sort-cards-trump (:trump game)))

(defn sort-cards [game]
  (update-game game sort-cards-player))




(when *debug* (println "fifth"))

(defn score-round [game]
  (let [bidder (:bidder game)
        bidteam (team bidder)
        bid (first (get-in game [:bids bidder]))
        bidscore (+ (get-in game [:points bidder]) (get-in game [:points (partner bidder)]))
        op (next-player bidder)
        opteam (team op)
        opscore (+ (get-in game [:points op]) (get-in game [:points (partner op)]))
        game  (update-in game [:score opteam] + opscore)]
    ;; scored opponent already and updated game
    (if bid
      (if (>= bidscore bid)
        (update-in game [:score bidteam] + bidscore)
        (update-in game [:score bidteam] - bid))
      game)))
    

(defn prepare-next-round [game]
  (-> game
      (update-in [:dealer] next-player)
      (assoc :deck (shuffle ordered-cards)
             :hands (nvec 4 [])
             :kept (nvec 4 -1)
             :burn (nvec 4 0)
             :leader []
             :tricks []
             :played []
             :trump nil
             :bids (nvec 4 [0 nil])
             :bidder 0
             :points (nvec 4 0))))


(defn play-half-round [game]
  ;; just for testing
  (-> game
      (deal)
      (take-bids)
      (declare-trump)
     
      (drop-non-trump)
      (deal-new-cards)
      (sort-cards)
      ;; burning extra trump is my simplification -- really should play multiple cards on
      ;; first trick
      (burn-extra-trump)

      ;; (tap prg "pre-tricks play-round")
      (play-trick)
      (play-trick)
      (play-trick)
      
      ;; stop in the middle for testing

      (tap prg "played half round")
      ))
  

(defn play-round [game]
  (-> game
      (deal)
      (take-bids)
      (declare-trump)
     
      ;;(tap prg "declare trump play-round")
      
      (drop-non-trump)
      (deal-new-cards)
      (sort-cards)
      ;; burning extra trump is my simplification -- really should play multiple cards on
      ;; first trick
      (burn-extra-trump)

      ;; (tap prg "pre-tricks play-round")
      (play-trick)
      (play-trick)
      (play-trick)
      (play-trick)
      (play-trick)
      (play-trick)
      (score-round)

      (tap prg "end of play-round")

      (prepare-next-round)
      ))


(defn run-game []
    (loop [game (new-game) winner nil]
      (let [[scoreNS scoreEW] (:score game)]

        ;; (tap game prg "in run-game")

        (cond winner (println "We have a winner! " (if (and winner (zero? winner)) "North-South" "East-West"))
              (and (>= scoreNS winning-score) (>= scoreEW winning-score)) (recur game (team (:bidder game)))
              (> scoreNS winning-score)  (recur game 0)
              (> scoreEW winning-score) (recur game 1)
              :else (recur (play-round game) nil)))))
              

(defn h [game p]
  (map cardstr (get-in game [:hands p])))

(defn pcs [cards]
  (println (map cardstr cards)))

;; ideas
;; - change cards to map :rank :suit for better conversion to record
;; - change hand rep to the map by :suit with :rank ordered for easier analysis
;; - change :suit to :trump after bids, would simplify things, really have 5 effective
;; suits
