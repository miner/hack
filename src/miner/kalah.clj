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

(defn finished? [game]
  (or (every? zero? (subvec (:pits game) 0 6))
      (every? zero? (subvec (:pits game) 7 13))))


(def opp-pit (merge (zipmap [0 1 2 3 4 5] [12 11 10 9 8 7])
                    (zipmap [12 11 10 9 8 7] [0 1 2 3 4 5])))

(def home-store (into (into {nil 6} (map #(vector % 6) (range 6)))
                      (map #(vector % 13) (range 7 13))))

(def opp-store  (into (into {nil 13} (map #(vector % 13) (range 6)))
                      (map #(vector % 6) (range 7 13))))

(defn play-last [pit game]
  (let [opp (opp-pit pit)
        opp-cnt (if opp (get-in game [:pits opp]) 0)
        store (home-store (peek (:turns game)))]
    (cond (= pit store) (assoc game :bonus true)
          (and (= 1 (get-in game [:pits pit])) (pos? opp-cnt))
              (-> game
                  (assoc-in [:pits pit] 0)
                  (assoc-in [:pits opp] 0)
                  (update-in [:pits store] + (inc opp-cnt)))
          :else game)))

(defn play-when-final [game]
  (let [sv6 (subvec (:pits game) 0 6)
        sv13 (subvec (:pits game) 7 13)]
    (cond (every? zero? sv6)
              (-> game
                  (dissoc :pits)
                  (assoc :final [(get-in game [:pits 6])
                                 (reduce + (get-in game [:pits 13]) sv13)]))
          (every? zero? sv13)
              (-> game
                  (dissoc :pits)
                  (assoc :final [(reduce + (get-in game [:pits 6]) sv6)
                                 (get-in game [:pits 13])]))
          :else game)))
        

(defn play-pit [game pit]
  (if (:final game)
    game
    (let [oppst (opp-store pit)
          seedcnt (get-in game [:pits pit])]
      (when (pos? seedcnt)
        (loop [p (inc pit)
               seeds seedcnt
               g (-> game
                     (dissoc :bonus)
                     (assoc-in [:pits pit] 0)
                     (update :turns conj pit))]
          (cond (zero? seeds) (play-when-final (play-last (if (zero? p) 13 (dec p)) g))
                (= p oppst) (recur (inc p) seeds g)
                (= p 14) (recur 0 seeds g)
                :else (recur (inc p) (dec seeds) (update-in g [:pits p] inc))))))))


;; util
(defn which-turn [game]
  (let [prev (peek (:turns game))]
    (home-store (if (:bonus game) prev (opp-pit prev)))))

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

    

(defn play-all-round [g]
  (let [prev (peek (:turns g))
        rng (if-not prev
              (range 6)
              (if (:bonus g)
                (if (< prev 6) (range 6) (range 7 13))
                (if (< prev 6) (range 7 13) (range 6))))]
  (keep #(play-pit g %) rng)))



(defn play-deep-round [g]
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
