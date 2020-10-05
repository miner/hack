(ns miner.ebwolf
  (:use miner.bitset))

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
(def bits [:boat :sheep :wolf :cabbage])

(def bi (merge {0 0 1 1 2 2 3 3} (zipmap bits (range))))

(def fields [:return :across :possible-passengers :after])

(def bf (merge {:null 0 0 0 1 4 2 8 3 12} (zipmap fields (iterate #(+ (count bits) %) 0))))



;; case doesn't seem to be any faster than simple map
(defn cbi [kcharacter]
  (case kcharacter
    :boat 0
    :sheep 1
    :wolf 2
    :cabbage 3))

(defn cbf [kfield]
  (case kfield
    :return 0
    :across 4
    :possible-passengers 8
    :after 12))






;; a macro that takes one or two kw and returns long index
;; [macro expansion does calc at compile-time]
(defmacro XXXfldind
  (^long [kf] (if (keyword? kf) (bf kf) `(bf ~kf)))
  (^long [kf kb]
   (if (and (keyword? kf) (keyword? kb))
     (+ (bf kf) (bi kb))
     `(+ (bf ~kf) (bi ~kb)))))

;; SEM FIXME  -- macros all the way down if you want to optimize offset calcs!!!
;; let's see if we can get it working for now.
;; SEM FIXME -- future idea is optimize macro that looks for constants and pre-calcs them



;; SEM FIXME use regular function for now, more convenient
(defn fldind
  (^long [kf] (bf kf))
  (^long [kf kb] (+ (bf kf) (bi kb))))



(defn bit? [state kfield kbit]
  (bit-test state (fldind kfield kbit)))

(defn setbit [state kfield kbit]
  (bit-set state (fldind kfield kbit)))

(defn clrbit [state kfield kbit]
  (bit-clear state (fldind kfield kbit)))

(defn field [state kfield]
  (bit-and 0xF (bit-shift-right state (fldind kfield))))

(defn setfield [state kfield val]
  (let [shift (fldind kfield)]
    (bit-or (bit-and-not state (bit-shift-left 0xF shift))
            (bit-shift-left val shift))))




(def all #{:boat :sheep :wolf :cabbage})

(def opposite-dir {:across :return :return :across})

(defn boat-side [state]
  (if (bit-test state 0)
    :return
    :across))


;; two sides of river, same names as direction for simplicity
;; everybody starts on the :return side and moves to the :across side
;; move {:direction :across/:return :passenger (one-of all)}

;; generic state
;;  {:return #{characters} :across #{characters} :after passenger :possible-passengers (remainers)}
;; initial {:return all :across #{}}
;; goal {:return #{} :across all}

;; the state :after says who moved to get to this state (i.e. after this passenger arrived)


(defn illegal-group? [group]
  (and (bit? group :null :sheep)
       (or (bit? group :null :wolf)
           (bit? group :null :cabbage))))

(defn legal-passengers [boat-group]
  (reduce (fn [group pbit]
            (if (illegal-group? (bit-clear boat-group pbit))
              (bit-clear group pbit)
              group))
          boat-group
          (bindices boat-group)))

(defn solution? [state]
  (= 0xF0 (bit-and 0xFF state)))

;; returns new stack with first move of state executed if possible.
;; if no moves left, state is discarded
;; if move creates a duplicated state (checking :return group), move is discarded
(defn execute-first-move [state stack]
  (when (> (count stack) 200) (throw (ex-info "Stack overflow" {:stack stack})))
  (if-let [passenger (high-bit-index (field state :possible-passengers))]
    (let [boat-dir (boat-side state)
          dir (opposite-dir boat-dir)
          new-state (-> state
                        (setfield :after (bit-shift-left 1 passenger))
                        (clrbit boat-dir :boat)
                        (bit-clear (+ (bf boat-dir) passenger))
                        (setbit dir :boat)
                        (bit-clear (+ (bf dir) passenger)))
          ret (field new-state :return)]
      (if (some #(= ret (field :return %)) stack)
        (conj stack (clrbit state :possible-passengers passenger))
        (conj stack
              (clrbit state :possible-passengers passenger)
              (setfield new-state :possible-passengers
                        (legal-passengers (field new-state dir))))))
    stack))

(defn create-move [i state]
  {:direction (if (even? i) :across :return)
   :passenger (let [pass (high-bit-index (field state :after))]
                (when-not (zero? pass)
                  (nth bits pass)))})

(defn create-solution [stack]
  (pop (into [] (map-indexed create-move) (rseq stack))))

;; returns list of solutions (vectors of moves)
(defn wsc []
  (loop [stack [(setfield 0xF :possible-passengers (legal-passengers 0xF))]
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
          boatside (boat-side state)
          dest (opposite-dir boatside)
          remainers (-> (field state boatside)
                        (clrbit :null :boat)
                        (clrbit :null passenger))]
      (when (and (= dest (:direction move))
                 (bit? state boatside passenger)
                 (not (illegal-group? remainers)))
        (-> state
            (setfield boatside remainers)
            (clrbit dest :boat)
            (clrbit dest passenger))))))
    

(defn wsc-valid? [moves]
  (and (= (map :direction moves) (take (count moves) (cycle [:across :return])))
       (solution? (reduce execute-move 0xF moves))))



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

