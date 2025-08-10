(ns miner.kalah)



;;; blog post:   https://vlaaad.github.io/kalaha
;;; source:    https://gist.github.com/vlaaad/5c7e1e44a34534840403ec6c27dcc7f6

;;; Rules
;;; https://en.wikipedia.org/wiki/Kalah


;;; orig
;; (ns io.github.vlaaad.kalaha)

(def empty-board
  (let [empty-player-data {:score 0
                           :cells [4 4 4 4 4 4]}]
    {:black empty-player-data
     :white empty-player-data
     :turn :white}))

(defn enemy [of]
  {:pre [(or (= of :black) (= of :white))]
   :post [(or (= % :black) (= % :white)) (not= of %)]}
  (case of
    :black :white
    :white :black))

(defn can-move? [board index]
  {:pre [(<= 0 index 5)]}
  (let [player (:turn board)
        points (get-in board [player :cells index])]
    (pos? points)))

(defn move [board index]
  {:pre [(can-move? board index)]}
  (let [player (:turn board)
        points (get-in board [player :cells index])]
    (loop [target player
           points points
           board (assoc-in board [player :cells index] 0)
           index (inc index)]
      (cond
        (zero? points)
        (cond
          ;; player finished by getting a score, he gets to make another move
          (and (zero? index) (not= target player))
          board

          ;; keep going?
          (and (pos? index) (< 1 (get-in board [target :cells (dec index)])))
          (let [index (dec index)
                points (get-in board [target :cells index])]
            (recur target points (assoc-in board [target :cells index] 0) (inc index)))

          :else
          (update board :turn enemy))

        ;; add point to own score?
        (and (= index 6) (= target player))
        (recur (enemy target) (dec points) (update-in board [player :score] inc) 0)

        ;; loop to us?
        (= index 6)
        (recur (enemy target) points board 0)

        ;; the usual
        :else
        (recur target (dec points) (update-in board [target :cells index] inc) (inc index))))))

(defn all-moves [board]
  (let [player (:turn board)]
    ((fn f [board acc]
       (eduction
         (mapcat
           (fn [index]
             (when (can-move? board index)
               (let [board (move board index)
                     acc (conj acc index)]
                 (if (= player (:turn board))
                   (f board acc)
                   [[board acc]])))))
         (range 0 6)))
     board [])))

;; The highest-scoring move:
(defn kalaha-high []
  (first (sort-by (comp - :score :white first) (vec (all-moves empty-board)))))


;;; ----------------------------------------------------------------------
;;; SEM variant

;;; Rules from Wiki
;; 
;; At the beginning of the game, four seeds are placed in each house.
;; 
;; Each player controls the six houses and the seeds on their side of the board. The player's
;; score is the number of seeds in the store to their right.
;; 
;; Players take turns sowing their seeds. On a turn, the player removes all seeds from one of
;; the houses under their control. Moving counter-clockwise, the player drops one seed in each
;; house in turn, including the player's own store but not their opponent's.
;; 
;; If the last sown seed lands in an empty house owned by the player, and the opposite house
;; contains seeds, both the last seed and the opposite seeds are captured and placed in the
;; player's store.
;; 
;; If the last sown seed lands in the player's store, the player gets an additional move. There
;; is no limit on the number of moves a player can make in their turn.
;; 
;; When one player no longer has any seeds in any of their houses, the game ends. The other
;; player moves all remaining seeds to their store, and the player with the most seeds in their
;; store wins.

;;; six pits "houses" + one large "store" (on the right) for each player on opposite sides
;;; :white 0-5, 6; :black 7-12, 13
;;; game state {:pits [14] :turns [pit]}


;;     12 11 10 9 8 7
;; 13                   6
;;     0  1  2  3 4 5 
;; 

(def init-game {:turns [] :pits (-> [] (into (repeat 6 4)) (conj 0)
                                    (into (repeat 6 4)) (conj 0))})

(defn NOT_USED_finished? [game]
  (or (every? zero? (subvec (:pits game) 0 6))
      (every? zero? (subvec (:pits game) 7 13))))


(def opp-pit (merge (zipmap [0 1 2 3 4 5] [12 11 10 9 8 7])
                    (zipmap [12 11 10 9 8 7] [0 1 2 3 4 5])))

(def home-store (into (into {nil 6} (map #(vector % 6) (range 7)))
                      (map #(vector % 13) (range 7 14))))

(def opp-store  (into (into {nil 13} (map #(vector % 13) (range 7)))
                      (map #(vector % 6) (range 7 14))))

(defn play-last [pit game]
  (let [opp (opp-pit pit)
        opp-cnt (if opp (get-in game [:pits opp]) 0)
        store (home-store (peek (:turns game)))]
    (cond (= pit store) (assoc game :bonus true)
          (and (or (and (= store 6) (< pit 6))
                   (and (= store 13) (<= 7 pit 12)))
               (= 1 (get-in game [:pits pit])) (pos? opp-cnt))
              (-> game
                  (assoc :capture pit)
                  (assoc-in [:pits pit] 0)
                  (assoc-in [:pits opp] 0)
                  (update-in [:pits store] + (inc opp-cnt)))
          :else game)))


(defn play-when-final [game]
  (let [sv7 (subvec (:pits game) 0 7)
        sv14 (subvec (:pits game) 7 14)]
    (if (or (every? zero? (pop sv7)) (every? zero? (pop sv14)))
      (-> game
          (dissoc :pits)
          (dissoc :bonus)
          (assoc :final [(reduce + 0 sv7)
                         (reduce + 0 sv14)]))
      game)))


(defn check-final [pit game]
  (play-when-final (play-last pit game)))

(defn inc6 [^long n]
  (case n
    12 0
    13 0
    (inc n)))

(defn dec6 [^long n]
  (case n
    0 12
    (dec n)))

(defn inc13 [^long n]
  (case n
    5 7
    13 0
    (inc n)))

(defn dec13 [^long n]
  (case n
    0 13
    7 5
    (dec n)))


(defn play-pit [game pit]
  (assert (int? pit))
  (if (:final game)
    game
    (let [incx (if (< pit 6) inc6 inc13)
          decx (if (< pit 6) dec6 dec13)
          seedcnt (get-in game [:pits pit])]
      (when (pos? seedcnt)
        (loop [p (incx pit)
               seeds seedcnt
               g (-> game
                     (dissoc :capture)
                     (dissoc :bonus)
                     (assoc-in [:pits pit] 0)
                     (update :turns conj pit))]
          (if (zero? seeds)
            (check-final (decx p) g)
            (recur (incx p) (dec seeds) (update-in g [:pits p] inc))))))))


(defn play-deep-pits1 [game rng]
  (if (empty? rng)
    [game]
    (let [gs (keep #(play-pit game %) rng)]
      (concat (remove :bonus gs)
              (mapcat (fn [g] (play-deep-pits1 g rng)) (filter :bonus gs))))))


(defn play-deep-pits [game rng]
  (reduce (fn [res g]
            (if (:bonus g)
              (into res (play-deep-pits g rng))
              (conj res g)))
          nil
          (keep #(play-pit game %) rng)))

;; util
(defn which-turn [game]
  (if-let [prev (peek (:turns game))]
    (home-store (if (:bonus game) prev (opp-pit prev)))
    6))

(defn sum-pits [g home]
  (let [pitv (if (= home 6) (subvec (:pits g) 0 7) (subvec (:pits g) 7 14))]
    (reduce + 0 pitv)))
    

(defn pick-turn [g]
  (let [homest (which-turn g)
        house-nums (if (= homest 6) (range 0 6) (range 7 13))
        allpits (if (= homest 6) (range 0 7) (range 7 14))
        results (keep #(play-pit g %) house-nums)
        score-diff (if (= homest 6)
                     #(if-let [[a b] (:final %)] (- a b) -49)
                     #(if-let [[a b] (:final %)] (- b a) -49))
        sum-score #(sum-pits % homest)
        wins (filter #(pos? (score-diff %)) results)]
   (if (seq wins)
     (apply max-key score-diff wins)
     (let [res (remove :final results)
           extras (filter :bonus res)]
       (if (seq extras)
         (apply max-key sum-score extras)
         (if (seq res)
           (apply max-key sum-score res)
           (rand-nth results)))))))

;;; strategy: might be better to pick best home score as opponent can't capture that.  Also,
;;; minimize the opponent's opportunity, not just max your own score.  So you need to model
;;; what the opponent would do.

;;; two opponents could use different strategies

;;; need better picker.  multiples scores, and tie-breakers

(defn max-score [scorefn coll]
  (reduce (fn [xs-high x]
            (let [high (peek xs-high)
                  sc (scorefn x)]
              (if (> sc high)
                [x sc]
                (if (= sc high)
                  (conj (pop xs-high) x sc)
                  xs-high))))
          [Long/MIN_VALUE]
          coll))

(defn maximize [coll score-fns]
  (let [cnt (count coll)]
    (case cnt
      0 nil
      1 (first coll)
      (if-let [score-fn (first score-fns)]
        (let [xshigh (max-score score-fn coll)]
          (recur (pop xshigh) (rest score-fns)))
        (first coll)))))
  



(defn best-turn [game]
  (let [homest (which-turn game)
        rng (if (= homest 6) (range 6) (range 7 13))
        possibles (play-deep-pits game rng)]
    (maximize possibles (if (= homest 6)
                          [#(if-let [[a b] (:final %)] (if (> a b) (- a b) -49) -49)
                           #(reduce + (subvec (:pits %) 0 7))
                           #(get-in % [:pits 6]) ]
                          [#(if-let [[a b] (:final %)] (if (> b a) (- b a) -49) -49)
                           #(reduce + (subvec (:pits %) 7 14))
                           #(get-in % [:pits 13]) ]))))


(defn run-game []
  (loop [g init-game cnt 300]
    (if (zero? cnt)
      [:time-out g]
      (if (:final g)
        g
        (recur (best-turn g) (dec cnt))))))




(defn run-game1 []
  (loop [g init-game cnt 300]
    (if (zero? cnt)
      [:time-out g]
      (if (:final g)
        g
        (recur (pick-turn g) (dec cnt))))))






(defn best-start []
  (loop [res [] gs [(assoc init-game :bonus true)]]
    (if (seq gs)
      (recur (into res (remove :bonus) gs)
             (mapcat (fn [g] (keep #(play-pit g %) (range 6))) (filter :bonus gs)))
      (sort-by (comp - #(get-in % [:pits 6]))  res))))
    

(defn sort-moves [gstates]
  (loop [res [] gs (mapv #(assoc % :bonus true) gstates)]
    (if (seq gs)
      (recur (into res (remove :bonus) gs)
             (mapcat (fn [g] (keep #(play-pit g %) (if (< (or (peek (:turns g)) 0) 6)
                                                    (range 6)
                                                    (range 7 13))))
                     (filter :bonus gs)))
      (sort-by (comp - #(get-in % [:pits (home-store (peek (:turns %)))])) res))))

    



(defn play-deep-round [game]
  (if (:final game)
    game
    (let [prev (peek (:turns game))
          rng (if (and prev (< prev 6)) (range 7 13) (range 6))]
      (reduce (fn [res g]
                (if (:bonus g)
                  (into res (play-deep-pits g rng))
                  (conj res g)))
              nil
              (keep #(play-pit game %) rng)))))


(defn play-deep-round1 [g]
  (let [prev (peek (:turns g))
        rng (if-not prev
              (range 6)
              (if (:bonus g)
                (if (< prev 6) (range 6) (range 7 13))
                (if (< prev 6) (range 7 13) (range 6))))
        round (keep #(play-pit g %) rng)]
    (into (remove :bonus round) (mapcat play-deep-round (filter :bonus round)))))


(defn play-deep-best [g]
  (let [all (play-deep-round g)
        homest (home-store (peek (:turns g)))]
    (apply max-key #(get-in % [:pits homest]) all)))




(defn play-deep-few
  ([] (play-deep-few [init-game]))
  ([gs]
   (concat (filter :final gs)
           (mapcat (fn [g]
                     (let [all (play-deep-round g)
                           homest (home-store (peek (:turns g)))]
                       (concat (filter :final all)
                               (take 3 (sort-by #(- (get-in % [:pits homest]))
                                                (remove :final all))))))
                   (remove :final gs)))))


;;; not practical to go beyond 5 deep rounds
;;; looks like you need to prune

;;; look for minimax

(defn kheuristic1 [g homest]
  (if-let [[a b] (:final g)]
    (if (= a b)
      0
      (if (= homest 6)
        (if (> a b) (* 100 (- a b)) -99)
        (if (> b a) (* 100 (- b a)) -99)))
    (let [pits (:pits g)
          sv7 (subvec pits 0 7)
          sv13 (subvec pits 7 13)
          score7 (reduce + (* 3 (peek sv7)) sv7)
          score13 (reduce + (* 3 (peek sv13)) sv13)]
      (if (= homest 6)
        (- score7 score13)
        (- score13 score7)))))


(defn kh6 [g]
  (if-let [[a b] (:final g)]
    (* 1000 (- a b))
    (let [pits (:pits g)
          sv7 (subvec pits 0 7)
          sv13 (subvec pits 7 13)
          score7 (reduce + (* 2 (peek sv7)) sv7)
          score13 (reduce + (* 2 (peek sv13)) sv13)]
      (- score7 score13))))

(def kh13 (comp - kh6))

(defn kheuristic [g]
  (let [last-pit (or (peek (:turns g)) 0)
        kh (kh6 g)]
    (if (< last-pit 6)
      kh
      (- kh))))

        
;;; default depth 4 is pretty fast but result is [41 7]
;;; default depth 5 is slow but closer result [22 26]


;;; alpha-beta from  https://clojurepatterns.com/17/3/23/
;;; slightly hacked by SEM
(defn alpha-beta-fn [terminal-node? heuristic-fn generate-children]
  (fn alpha-beta
    ([node] (alpha-beta node 4 Long/MIN_VALUE Long/MAX_VALUE true))
    ([node depth alpha beta maximizing?]
    (if (or (zero? depth) (terminal-node? node))
      (heuristic-fn node)
      (if maximizing?
        (loop [value Long/MIN_VALUE
               children (generate-children node)]
          (if (empty? children)
            value
            (let [child (first children)
                  value (max value (alpha-beta child (dec depth) alpha beta false))]
              (if (>= value beta)
                value
                (recur (long (max alpha value)) (rest children))))))
        (loop [value Long/MAX_VALUE
               children (generate-children node)]
          (if (empty? children)
            value
            (let [child (first children)
                  value (min value (alpha-beta child (dec depth) alpha beta true))]
              (if (<= value alpha)
                value
                (recur (long (min beta value)) (rest children)))))))))))


(def kalah-ab (alpha-beta-fn :final kheuristic play-deep-round))

(defn run-kalah []
  (loop [g (apply max-key kalah-ab (play-deep-round init-game))
         limit 200]
    (if (zero? limit)
      [:limit g]
      (if (:final g)
        g
        (recur (apply max-key kalah-ab (play-deep-round g)) (dec limit))))))



