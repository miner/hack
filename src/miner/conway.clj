(ns miner.conway)

;;; original by Cristophe Grand
;;; http://clj-me.cgrand.net/2011/08/19/conways-game-of-life/


(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])] 
    [(+ dx x) (+ dy y)]))

(defn step [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(def board #{[1 0] [1 1] [1 2]})
; #'user/board

#_
(take 5 (iterate step board))

;; (#{[1 0] [1 1] [1 2]} #{[2 1] [1 1] [0 1]} #{[1 0] [1 1] [1 2]} #{[2 1] [1 1] [0 1]}
;; #{[1 0] [1 1] [1 2]})


;;; An implementation with quil display
;;; https://github.com/jackdbd/game-of-life
