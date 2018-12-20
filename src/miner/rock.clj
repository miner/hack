(ns miner.rock)

;; yet another rock, paper, scissors

;; See also
;; core.async version by Alex Miller, https://gist.github.com/puredanger/5965883
;; apropos_cast #23 video https://www.youtube.com/watch?v=_lqRqvrpuhw
;; Rosetta Code: https://rosettacode.org/wiki/Rock-paper-scissors#Clojure


(def dominator {:rock :paper, :scissors :rock, :paper :scissors})

(def choices (vec (keys dominator)))


(defn wins [a b]
  (cond (= a b) nil
        (nil? a) 1
        (nil? b) 0
        (= a (dominator b)) 0
        (= b (dominator a)) 1
        :else nil))

(defn favorite [game-state player]
  (let [choices (get-in game-state [player :history])]
    (first (reduce-kv (fn [rkv k v] (if (> v (peek rkv)) [k v] rkv))
                      [nil 0] 
                      (frequencies choices)))))


;; history is sequence of choices [p0 p1 p0 p1 ...]  each pair being one round
;; 

;; game-state
;; [{:strategy fn-game-state :history [choices...]}
;;  {:strategy fn-game-state :history [choices...]}]


(defn opponent [player]
  (if (zero? player) 1 0))

(defn guess-random [player game-state]
  (rand-nth choices))
  
(defn guess-rock [player game-state]
  :rock)

(defn guess-common [player game-state]
  (let [cnt (count (get-in game-state [player :history]))]
    (if (< cnt 5)
      (rand-nth choices)
      (let [fav (favorite game-state (opponent player))]
        (or (dominator fav) (rand-nth choices))))))

(defn guess-vizzini [player game-state]
  (let [cnt (count (get-in game-state [player :history]))]
    (if (< cnt 10)
      (rand-nth choices)
      (dominator (dominator (favorite game-state player))))))


;; try to mix up my choice if he assumes I'm the same
(defn guess-diff [player game-state]
  (let [prev (peek (get-in game-state [player :history]))]
    (or (dominator prev)
        (rand-nth choices))))

;; guess opponent will repeat
(defn guess-prev [player game-state]
  (let [prev (peek (get-in game-state [(opponent player) :history]))]
    (or (dominator prev)
        (rand-nth choices))))

;; whatever opponent did last time
(defn guess-same [player game-state]
  (or (peek (get-in game-state [(opponent player) :history]))
      (rand-nth choices)))

(defn step-game [game-state]
  (let [p0 ((get-in game-state [0 :strategy]) 0 game-state)
        p1 ((get-in game-state [1 :strategy]) 1 game-state)]
    (-> game-state
        (update-in [0 :history] conj p0)
        (update-in [1 :history] conj p1))))

(defn report-game [game-state]
  (let [freq (frequencies (map wins
                               (get-in game-state [0 :history])
                               (get-in game-state [1 :history])))]
    (with-meta
      [(freq 0 0) (freq 1 0) (freq nil 0)]
      {:game game-state})))



(defn run-game
  ([] (run-game 100))
  ([rounds] (run-game guess-random guess-random rounds))
  ([strat0 strat1] (run-game strat0 strat1 100))
  ([strat0 strat1 rounds]
   (loop [n 100 game-state [{:strategy strat0 :history []}
                            {:strategy strat1 :history []}]]
     (if (zero? n)
       (report-game game-state)
       (recur (dec n) (step-game game-state))))))



