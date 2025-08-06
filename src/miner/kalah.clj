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
(defn kalaha-high
  (first (sort-by (comp - :score :white first) (vec (all-moves empty-board)))))

