(ns miner.enormandwsc)

;;; Wolf Sheep Cabbage

(def bad-pairs #{#{:wolf :sheep}
                 #{:sheep :cabbage}})

(def starting-state {:near #{:wolf :sheep :cabbage :me}
                     :far #{}})
(def ending-state {:near #{}
                   :far #{:wolf :sheep :cabbage :me}})

(defn lose? [state]
  (if (contains? (:near state) :me)
    (some #(clojure.set/subset? % (:far  state)) bad-pairs)
    (some #(clojure.set/subset? % (:near state)) bad-pairs)))

(defn win? [state]
  (= ending-state state))

(defn current-location [state]
  (if (contains? (:near state) :me)
    :near :far))

(defn opposite-location [state]
  (if (contains? (:near state) :me)
    :far :near))

(defn next-direction [state]
  (if (contains? (:near state) :me)
    :across :return))

(defn neighbors [state]
  (-> state
      (get (current-location state))
      (disj :me)
      (conj :none)))

(defn next-moves [state]
  (for [n (neighbors state)]
    {:direction (next-direction state)
     :passenger n}))

(defn make-move [state {:keys [direction passenger]}]
  (-> state
      (update (current-location  state) disj :me passenger)
      (update (opposite-location state) #(if (= :none passenger)
                                           (conj % :me)
                                           (conj % :me passenger)))))

(defn try-moves [moves]
  (for [move (next-moves (:state moves))]
    (-> moves
        (update :moves conj      move)
        (update :state make-move move))))

(defn winning-sequences []
  (loop [contenders [{:state starting-state :moves []}]
         tried-states #{starting-state}
         winners []]
    (if (empty? contenders)
      (map :moves winners)
      (let [possible-contenders (mapcat try-moves contenders)
            next-winners (filter #(win? (:state %)) possible-contenders)
            next-contenders (->> possible-contenders
                                 (remove #(win?  (:state %)))
                                 (remove #(lose? (:state %)))
                                 (remove #(contains? tried-states (:state %))))]
        (recur next-contenders
               (into tried-states (map :state possible-contenders))
               (into winners next-winners))))))



