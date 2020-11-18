(ns miner.etictac)

;; Aprops tic-tac-toe
;; https://youtu.be/rRs7C2wrqgw

;; my version, not theirs


;;; actually, it would be better to go odd and even in move order

;;;;;;;; ODD/EVEN
;; Even move is X, Odd move is O

(def init (vec (repeat 9 nil)))

(def xxx [0 2 4 1 3 nil nil nil nil])

(def ooo [0 nil 1 nil 3 2 5 4 nil])


(defn print-state [state]
  (doseq [row (range 0 7 3)]
    (println)
    (doseq [col (range 3)]
      (print " ")
      (if-let [v (nth state (+ row col))]
        (print (str (if (odd? v) "O" "X") v))
        (print  "--"))))
  (println)
  (println))

(defn win3 [abc]
  (when (and (not-any? nil? abc)
             (apply = (map odd? abc)))
    (if (odd? (first abc))
      :O
      :X)))

(defn winner [state]
  (or (some win3 (partition 3 state))
      (win3 (take-nth 3 state))
      (win3 (take-nth 3 (rest state)))
      (win3 (take-nth 3 (nnext state)))
      (win3 (mapv #(nth state %) [0 4 8]))
      (win3 (mapv #(nth state %) [2 4 6]))))

(defn moves [state]
  (seq (keep-indexed (fn [i v] (when (nil? v) i)) state)))

(defn last-move [state]
  (reduce max -1 (remove nil? state)))


;; VERY WEAK PLAYER -- need to look at state more
;; need to look for two in a row (or simulate move and check winner)
;; score opponent as well
;; block opponent
;; probably should score 8 dimensions

(defn x-player [state]
  (first (moves state)))

(defn o-player [state]
  (let [mvs (moves state)
        sms (set mvs)]
    (when mvs
      (if (sms 4) 4 (rand-nth mvs)))))

(defn run-game
  ([] (run-game x-player o-player))
  ([x-player o-player]
  (loop [state initial-state]
    (print-state state)
    (or (winner state)
        (let [lst (last-move state)
              pmove  (if (odd? lst) x-player o-player)
              mv (pmove state)]
          (if mv
            (recur (assoc state mv (inc lst)))
            :draw))))))









