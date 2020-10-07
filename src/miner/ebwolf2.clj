(ns miner.ebwolf2
  (:require [miner.bitset :as bs]))

;; https://gist.github.com/ericnormand/3bcffa74da2cad97fcbfb908e347bcde

;; Eric's Challenge
;; Wolf-Sheep-Cabbage puzzle
;;

;; You own a wolf, a sheep, and a cabbage. You need to cross a river and you only have a
;; boat big enough for you plus one other passenger (your wolf, sheep, or cabbage). The
;; trouble is, the animals are hungry. If you leave the wolf with the sheep, the wolf will
;; eat it. If you leave the sheep with the cabbage, it will eat it. The wolf, being a
;; carnivore, will not eat the cabbage. How do you move them safely to the other side with
;; nothing being eaten?
;;
;; Your task is to write a function that generates solutions to this puzzle, in the form of
;; boat crossings. Each crossing should state: the direction you are going, and the
;; passenger in your boat.  Bonus: Write a function that validates a solution.

;; (defn wolf-sheep-cabbage []) => ([...
;;                                  {:direction :across
;;                                   :passenger :cabbage}
;;                                  {:direction :return
;;                                   :passenger :sheep}]
;;                                 ...)

;; inital state:  W S C b ---  [empty]
;; :across S,   W C --- b S
;; :return nil, W C b --- S
;;:across C,  W --- b C S
;; :return S,  W S b --- C
;; :across W,  S -- b W C
;; :return nil, S b -- W C
;; :across S, [empty] -- b S W C


;; Assume You and Boat are always together.  The animals eat something only if you leave
;; them alone together.  If you're there, everyone is safe.

;; "You" and the boat are essentially always together so we represent that as simply :boat.
;; Thus, having no passenger is the same as passager :boat.  Internally, the explicit :boat
;; seems more convenient than special handling for nil.  However, for the reported moves, I
;; think nil is clearer and therefore we don't expose :boat as a "passenger".


;; SEM new idea
;; state is vector of four booleans (index by character) + next move (int)
;;  across = true
;;  return (start) = false
;;
;; "move" is just index of character that moved
;; possible moves always start at highest (3 cabbage) and works down to (0 boat)
;; that should make it easy to reverse last move

;;; NEW NEW IDEA:  bits in a long instead of a vector
;;; next-move is lower 2 bits

;; Four characters:

(def boat 3)
(def sheep 4)
(def wolf 5)
(def cabbage 6)
(def move-mask 2r111)  ;; 3 bits
(def state-mask 2r1111111)  ;; 7 bits
(def cmask (bit-and-not state-mask move-mask))

(def opposite-dir {:across :return :return :across})

;; two sides of river, same names as direction for simplicity
;; everybody starts on the :return side and moves to the :across side
;; move {:direction :across/:return :passenger (one-of all)}


(defn across? [state i] (bit-test state i))

(defn return? [state i] (not (across? state i)))

(defn print-state
  ([state] (print-state " " state))
  ([msg state]
  (if-not state
    (println msg "NIL state")
    (println msg
             (keep (fn [i] (nth '[nil nil nil b s w c] i nil))
                   (bs/bindices (bit-xor cmask (bit-and-not state move-mask))))
             (keep (fn [i] (nth '[nil nil nil B S W C] i nil))
                   (bs/bindices (bit-and state cmask)))
             :next (nth '[nil nil nil B S W C] (bit-and state move-mask))))))


(defn illegal-state? [state]
  (cond (not state) true
        (across? state boat)   (and (return? state sheep)
                                    (or (return? state wolf)
                                        (return? state cabbage)))
        :else    (and (across? state sheep)
                      (or (across? state wolf)
                          (across? state cabbage)))))

(defn solution? [state]
  (= cmask (bit-and state cmask)))

(defn create-move [i state]
  {:direction (if (even? i) :across :return)
   :passenger (nth [nil nil nil nil :sheep :wolf :cabbage] (bit-and move-mask state))})

(defn create-solution [stack]
  (into [] (map-indexed create-move) (rseq stack)))

;; returns list of solutions (vectors of moves)
(defn wsc []
  (loop [stack [cabbage]
         ;; SEM FIXME assumes initial state is not a solution
         solutions nil]
    (if-let [state (peek stack)]
      (let [pass (bit-and move-mask state)]
        (if (< pass boat)
          (let [stack (pop stack)
                top (peek stack)]
            (if top
              (recur (conj (pop stack) (dec top)) solutions)
              solutions))

          (let [new-state (when (= (across? state boat) (across? state pass))
                            (-> state
                                (bit-flip boat)
                                (bit-and-not move-mask)
                                (bit-or cabbage)
                                (cond-> (not= pass boat) (bit-flip pass))))]
            (cond (illegal-state? new-state)
                      (recur (conj (pop stack) (dec state)) solutions)
                  
                  (some #(= (bit-and-not new-state move-mask) (bit-and-not % move-mask)) stack)
                      (recur (conj (pop stack) (dec state)) solutions)

                  (solution? new-state)
                      (recur (conj (pop stack) (dec state))
                             (conj solutions (create-solution stack)))

                  :else (recur (conj stack new-state) solutions)))))
      solutions)))


(def pass-index {:sheep sheep
                 :cabbage cabbage
                 :wolf wolf})

;; returns new state if move is legal, otherwise nil
(defn execute-move [state move]
  (let [pind (pass-index (:passenger move))
        boat-side (if (across? state boat) :across :return)
        pass-side (if pind (if (across? state pind) :across :return) boat-side)]
    (when (and (= (:direction move) (opposite-dir boat-side))
               (= boat-side pass-side))
      (-> state
          (bit-flip boat)
          (cond-> pind (bit-flip pind))))))

(defn wsc-valid? [moves]
  (and (= (map :direction moves) (take (count moves) (cycle [:across :return])))
       (solution? (reduce execute-move 0 moves))))



(defn smoke-wsc []
  (let [result (wsc)]
    (assert (= (count result) 2))
    (assert (every? wsc-valid? result))
    (assert (every? #{[{:direction :across, :passenger :sheep}
                       {:direction :return, :passenger nil}
                       {:direction :across, :passenger :cabbage}
                       {:direction :return, :passenger :sheep}
                       {:direction :across, :passenger :wolf}
                       {:direction :return, :passenger nil}
                       {:direction :across, :passenger :sheep}]
                      [{:direction :across, :passenger :sheep}
                       {:direction :return, :passenger nil}
                       {:direction :across, :passenger :wolf}
                       {:direction :return, :passenger :sheep}
                       {:direction :across, :passenger :cabbage}
                       {:direction :return, :passenger nil}
                       {:direction :across, :passenger :sheep}]}
                    result)))
  true)

