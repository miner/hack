(ns miner.ewolf)

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


;; Four characters:

(def all #{:boat :sheep :wolf :cabbage})

(def opposite-dir {:across :return :return :across})

;; two sides of river, same names as direction for simplicity
;; everybody starts on the :return side and moves to the :across side
;; move {:direction :across/:return :passenger (one-of all)}

;; generic state
;;  {:return #{characters} :across #{characters} :after passenger :possible-passengers (remainers)}
;; initial {:return all :across #{}}
;; goal {:return #{} :across all}

;; the state :after says who moved to get to this state (i.e. after this passenger arrived)


(defn illegal-group? [side]
  (and (:sheep side)
       (or (:wolf side)
           (:cabbage side))))

(defn legal-passengers [boat-group]
  (remove (fn [passenger] (illegal-group? (disj boat-group passenger))) boat-group))


(defn solution? [state]
  (= all (:across state)))

;; returns new stack with first move of state executed if possible.
;; if no moves left, state is discarded
;; if move creates a duplicated state (checking :return group), move is discarded
(defn execute-first-move [state stack]
  #_ (when (> (count stack) 200) (throw (ex-info "Stack overflow" {:stack stack})))
  (if-let [passenger (first (:possible-passengers state))]
    (let [dir (if (:boat (:return state)) :across :return)
          dest-group (conj (dir state) :boat passenger)
          new-state (-> state
                        (assoc :after passenger)
                        (update (opposite-dir dir) disj :boat passenger)
                        (assoc dir dest-group))
          ret (:return new-state)]
      (if (some #(= ret (:return %)) stack)
        (conj stack (update state :possible-passengers rest))
        (conj stack
              (update state :possible-passengers rest)
              (assoc new-state :possible-passengers
                     (legal-passengers dest-group)))))
    stack))

(defn create-move [i state]
  {:direction (if (even? i) :across :return)
   :passenger (let [pass (:after state)] (when (not= pass :boat) pass))})

(defn create-solution [stack]
  (pop (into [] (map-indexed create-move) (rseq stack))))

;; returns list of solutions (vectors of moves)
(defn wsc []
  (loop [stack [{:return all :across #{} :after nil
                 :possible-passengers (legal-passengers all)}]
         solutions nil]
    (if-let [state (peek stack)]
      (if (solution? state)
        (recur (pop stack) (conj solutions (create-solution stack)))
        (recur (execute-first-move state (pop stack)) solutions))
      solutions)))

;; returns new state if move is legal, otherwise nil
(defn execute-move [state move]
  (when state
    (let [passenger (or (:passenger move) :boat)
          boatside (if (:boat (:return state)) :return :across)
          dest (opposite-dir boatside)
          remainers (disj (boatside state) :boat passenger)]
      (when (and (= dest (:direction move))
                 (passenger (boatside state))
                 (not (illegal-group? remainers)))
        (-> state
            (assoc boatside remainers)
            (update dest conj :boat passenger))))))
    

(defn wsc-valid? [moves]
  (and (= (map :direction moves) (take (count moves) (cycle [:across :return])))
       (solution? (reduce execute-move {:return all :across #{}} moves))))



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

