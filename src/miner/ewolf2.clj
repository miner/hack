(ns miner.ewolf2)

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


;; Four characters:

(def boat 0)
(def sheep 1)
(def wolf 2)
(def cabbage 3)
(def next-move 4)


(def opposite-dir {:across :return :return :across})

;; two sides of river, same names as direction for simplicity
;; everybody starts on the :return side and moves to the :across side
;; move {:direction :across/:return :passenger (one-of all)}


(defn across? [state i] (true? (nth state i)))

(defn return? [state i] (not (across? state i)))

(defn print-state
  ([state] (print-state " " state))
  ([msg state]
  (if-not state
    (println msg "NIL state")
    (println msg
             (keep-indexed (fn [i x] (when-not x (nth '[b s w c] i))) (pop state))
             (keep-indexed (fn [i x] (when x (nth '[B S W C] i))) (pop state))
             :next (nth '[B S W C] (peek state) (peek state))))))


(defn illegal-state? [state]
  (cond (not state) true
        (across? state boat)   (and (return? state sheep)
                                    (or (return? state wolf)
                                        (return? state cabbage)))
        :else    (and (across? state sheep)
                      (or (across? state wolf)
                          (across? state cabbage)))))

(defn solution? [state]
  (every? true? (pop state)))

(defn create-move [i state]
  {:direction (if (even? i) :across :return)
   :passenger (nth [nil :sheep :wolf :cabbage] (peek state))})

(defn create-solution [stack]
  (into [] (map-indexed create-move) (rseq stack)))

;; returns list of solutions (vectors of moves)
(defn wsc []
  (loop [stack [[false false false false 3]]
         ;; SEM FIXME assumes initial state is not a solution
         solutions nil
         ctr 500]

    (when (zero? ctr) (throw (ex-info "Too many iterations" {:stack stack})))
    
    (println)
    (run! print-state stack)
    (println)
    (flush)
    (when (> (count stack) 20) (throw (ex-info "Stack overflow" {:stack stack})))

    (if-let [state (peek stack)]
      (let [pass (peek state)]
        (if (neg-int? pass)
          (do (println "  neg passenger")
              (let [stack (pop stack)
                    top (peek stack)]
                (if top
                  (recur (conj (pop stack) (update top next-move dec))
                         solutions (dec ctr))
                  (do (println "  exhausted")
                      solutions))))

          (let [new-state (when (= (state boat) (state pass))
                            (-> state
                                (update boat not)
                                (assoc next-move cabbage)
                                (cond-> (not= pass boat) (update pass not))))]
            (cond (illegal-state? new-state)
                      (do (print-state "  Illegal" new-state)
                          (println)
                          (recur (conj (pop stack) (update state next-move dec))
                                 solutions (dec ctr)))
                  
                  (some #(= (pop new-state) (pop %)) stack)
                      (do (print-state "  Repeated" new-state)
                          (println)
                          (recur (conj (pop stack) (update state next-move dec))
                                 solutions (dec ctr)))

                  (solution? new-state)
                      (do (println "solution " (create-solution stack))
                      (recur (conj (pop stack) (update state next-move dec))
                             (conj solutions (create-solution stack))
                             (dec ctr)))

                  :else (recur (conj stack new-state)
                               solutions (dec ctr))))))
      solutions)))




;; returns new state if move is legal, otherwise nil
(defn execute-move [state move]
  nil)


(defn wsc-valid? [moves]
  false)

;;  (and (= (map :direction moves) (take (count moves) (cycle [:across :return])))
;;       (solution? (reduce execute-move {:return all :across #{}} moves))))



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

