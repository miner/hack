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


;; :across W,  S --- b W C
;; :return nil, S b -- W C
;; :across S, [empty] -- b S W C

;; Assume You and Boat are always together.  The animals eat something only if you leave
;; them alone together.  If you're there, everyone is safe.

;; no passenger is that same as :passager :boat, and that seems the more convenient notation

;; Four characters:
(def characters [:boat :sheep :wolf :cabbage])

(def all (set characters))

(def eaters [#{:wolf :sheep} #{:sheep :cabbage}])

(def opposite-dir {:across :return :return :across})

(defn subset? [sub super]
  (every? super sub))

;; state {:return #{characters} :across #{characters}}

(defn illegal-state? [state]
  (let [start (:return state)
        destination (:across state)
        non-boat (if (:boat start) destination start)]
    (not-any? #(subset? % non-boat) eaters)))


(defn illegal-side? [side]
  (and (:sheep side)
       (or (:wolf side)
           (:cabbage side))))

(defn legal-moves [dir boatside]
  (vec (for [passenger boatside :when (not (illegal-side? (disj boatside passenger)))]
         {:direction dir
          :passenger passenger})))

(defn state-legal-moves [state]
  (let [start (:return state)
        destination (:across state)
        dir (if (:boat start) :across :return)
        boatside (if (:boat start) start destination)]
    (legal-moves dir boatside)))

(defn solution? [state]
  (= all (:across state)))

(defn execute-first-move [state stack]
  (if (> (count stack) 200)
    (throw (ex-info "Stack overflow" {:stack stack}))
    (let [possibles (:possible-moves state)
          move (peek possibles)]
      (if-not move
        stack
        (let [passenger (:passenger move)
              dir (:direction move)
              boat-side (conj (dir state) :boat passenger)
              new-state (-> state
                            (assoc :after move)
                            (update (opposite-dir dir) disj :boat passenger)
                            (assoc dir boat-side))]
          (if (some #(= (:return new-state) %) (map :return stack))
            (conj stack (update state :possible-moves pop))
            (conj stack
                (update state :possible-moves pop)
                (assoc new-state :possible-moves
                       (legal-moves (opposite-dir dir) boat-side)))))))))



;; Finds one solution and quits
(defn wsc []
  (loop [stack [{:return all :across #{} :after nil
                 :possible-moves (legal-moves :across all)}]
         solutions nil]
    (let [state (peek stack)]
      #_(println state)
      (cond (nil? state) solutions
            (solution? state)  (recur (pop stack)
                                      (conj solutions (pop (into [] (map :after) (rseq stack)))))
            :else (recur (execute-first-move state (pop stack)) solutions)))))


