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

;; no passenger is that same as :passager :boat, and that seems the more convenient notation

;; Four characters:

(def all #{:boat :sheep :wolf :cabbage})

(def opposite-dir {:across :return :return :across})

;; two sides of river, same as direction for simplicity
;; everybody starts on the :return side and moves to the :across side
;; move {:direction :across/:return :passenger (one-of all)}

;; generic state
;; {:return #{characters} :across #{characters} :after move :possible-moves (moves)}
;; initial {:return all :across #{}}
;; goal {:return #{} :across all}


;; SEM FIXME :direction is implied by where the :boat is so we don't really need to track it
;; We can recreate as we store a solution.

(defn illegal-side? [side]
  (and (:sheep side)
       (or (:wolf side)
           (:cabbage side))))

(defn legal-passengers [boatside]
  (remove (fn [passenger] (illegal-side? (disj boatside passenger))) boatside))


;; SEM FIXME a move could simply be a passenger, calculate the direction from state



(defn solution? [state]
  (= all (:across state)))

(defn execute-first-move [state stack]
  (if (> (count stack) 200)
    (throw (ex-info "Stack overflow" {:stack stack}))
    (let [possibles (:possible-passengers state)
          passenger (first possibles)]
      (if-not passenger
        stack
        (let [dir (if (:boat (:return state)) :across :return)
              boat-side (conj (dir state) :boat passenger)
              new-state (-> state
                            (assoc :after passenger)
                            (update (opposite-dir dir) disj :boat passenger)
                            (assoc dir boat-side))]
          (if (some #(= (:return new-state) %) (map :return stack))
            (conj stack (update state :possible-passengers rest))
            (conj stack
                (update state :possible-passengers rest)
                (assoc new-state :possible-passengers
                       (legal-passengers boat-side)))))))))

(defn create-move [i state]
  {:direction (if (even? i) :across :return)
   :passenger (let [pass (:after state)] (when (not= pass :boat) pass))})

;; Finds one solution and quits
(defn wsc []
  (loop [stack [{:return all :across #{} :after nil
                 :possible-passengers (legal-passengers all)}]
         solutions nil]
    (let [state (peek stack)]
      #_(println state)
      (cond (nil? state) solutions
            (solution? state)  (recur (pop stack)
                                      (conj solutions
                                            (pop (into [] (map-indexed create-move)
                                                       (rseq stack)))))
            :else (recur (execute-first-move state (pop stack)) solutions)))))





(defn execute-move [state move]
  (when state
    (let [passenger (or (:passenger move) :boat)
          boatside (if (:boat (:return state)) :return :across)
          dest (opposite-dir boatside)
          remainers (disj (boatside state) :boat passenger)]
      (when (and (= dest (:direction move))
                 (passenger (boatside state))
                 (not (illegal-side? remainers)))
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
    #_ (assert (every? #{[{:direction :across, :passenger :sheep}
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



;;; JUNK

(comment
  
(defn state-legal-moves [state]
  (let [start (:return state)
        destination (:across state)
        dir (if (:boat start) :across :return)
        boatside (if (:boat start) start destination)]
    (legal-moves dir boatside)))


(defn illegal-state? [state]
  (let [start (:return state)
        destination (:across state)
        non-boat (if (:boat start) destination start)]
    (not-any? #(subset? % non-boat) eaters)))


(defn subset? [sub super]
  (every? super sub))

(def eaters [#{:wolf :sheep} #{:sheep :cabbage}])

)
