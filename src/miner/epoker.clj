(ns miner.epoker)

;; Eric's challenge: poker evaluation
;; https://gist.github.com/ericnormand/bf1408eb11d4c9a36a786cab946ef0c2

;; Cards are represented as a tuple of rank (number or name if it's a face card) and
;; suit. Face card names are the keywords :ace, :king, :queen, :jack. Suits
;; are :diamonds, :spades, :hearts, :clubs.



;; My internal representation for a card is a two element vector of [rank suit] where rank
;; is a long 2 - 14 (Ace high), and suit is a keyword :hearts, :diamonds :clubs, or :spades.


(def valid-scores  #{:high-card :pair :two-pair :three-of-a-kind :straight
                     :flush :full-house :four-of-a-kind :straight-flush :royal-flush})

(def numeric-scores (into {}
                          (map-indexed #(vector %2 (* 100 (inc %))))
                          [:high-card :pair :two-pair :three-of-a-kind :straight
                           :flush :full-house :four-of-a-kind :straight-flush :royal-flush]))


(def ranks #{2 3 4 5 6 7 8 9 10 :jack :queen :king :ace})

(def suits #{:clubs :diamonds :hearts :spades})

(defn valid-card? [card]
  (and (vector? card)
       (= (count card) 2)
       (contains? ranks (nth card 0))
       (contains? suits (nth card 1))))

(defn valid-hand? [hand]
  (and (every? valid-card? hand)
       (= (count hand) 5)
       (distinct? hand)))



(defn nrank [card]
  (let [rk (nth card 0)]
    (case rk
      :ace   14
      :king  13
      :queen 12
      :jack  11
      rk)))

(defn score [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [sorted-ranks (sort > (map nrank hand))
        flush? (apply = (map peek hand))]
    (if (and flush? (= sorted-ranks [14 13 12 11 10]))
      :royal-flush
      (let [straight? (or (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
                          (= sorted-ranks [14 5 4 3 2]))]
        (cond (and straight? flush?) :straight-flush
              flush? :flush
              straight? :straight
              :else (let [kind-counts (sort > (vals (frequencies sorted-ranks)))
                          max-kind (first kind-counts)]
                      (cond (= max-kind 4) :four-of-a-kind
                            (= kind-counts [3 2]) :full-house
                            (= max-kind 3) :three-of-a-kind
                            (= kind-counts [2 2 1]) :two-pair
                            (= max-kind 2) :pair
                            :else :high-card)))))))



;; use the card ranks to make an extended score that can properly `compare` to others
(defn xscore [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [sorted-ranks (sort > (map nrank hand))
        flush? (apply = (map peek hand))
        hand-type (if (and flush? (= sorted-ranks [14 13 12 11 10]))
      :royal-flush
      (let [straight? (or (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
                          (= sorted-ranks [14 5 4 3 2]))]
        (cond (and straight? flush?) :straight-flush
              flush? :flush
              straight? :straight
              :else (let [kind-counts (sort > (vals (frequencies sorted-ranks)))
                          max-kind (first kind-counts)]
                      (cond (= max-kind 4) :four-of-a-kind
                            (= kind-counts [3 2]) :full-house
                            (= max-kind 3) :three-of-a-kind
                            (= kind-counts [2 2 1]) :two-pair
                            (= max-kind 2) :pair
                            :else :high-card)))))]
    (into [(numeric-scores hand-type)] sorted-ranks)))






(defn smoke-hands [score]
  (assert (= (score [[10 :diamonds] [:jack :diamonds] [:ace :diamonds] [:queen :diamonds]
                     [:king :diamonds]])
             :royal-flush))
  (assert (= (score [[10 :diamonds] [:jack :diamonds] [9 :diamonds] [:queen :diamonds]
                     [:king :diamonds]])
             :straight-flush))
  (assert (= (score [[10 :diamonds] [:jack :spades] [9 :diamonds] [:queen :diamonds]
                     [:king :diamonds]])
             :straight))
  (assert (= (score [[2 :diamonds] [:jack :diamonds] [:ace :diamonds] [:queen :diamonds]
                     [:king :diamonds]])
             :flush))
  (assert (= (score [[3 :diamonds] [4 :diamonds] [:ace :diamonds] [5 :diamonds]
                     [2 :diamonds]])
             :straight-flush))
  (assert (= (score [[3 :clubs] [4 :diamonds] [:ace :diamonds] [5 :diamonds]
                     [2 :diamonds]])
             :straight))
  (assert (= (score [[3 :clubs] [4 :diamonds] [:ace :diamonds] [5 :diamonds]
                     [7 :diamonds]])
             :high-card))
  (assert (= (score [[10 :diamonds] [:ace :clubs] [:ace :diamonds] [:queen :diamonds]
                     [10 :clubs]])
             :two-pair))

  (assert (= (score [[10 :diamonds] [:jack :diamonds] [:ace :diamonds] [:queen :diamonds]
                     [10 :clubs]])
             :pair))

  (assert (= (score [[10 :diamonds] [10 :spades] [10 :hearts] [:queen :diamonds]
                     [10 :clubs]])
             :four-of-a-kind))

  (assert (= (score [[10 :diamonds] [10 :spades] [2 :hearts] [:queen :diamonds]
                     [10 :clubs]])
             :three-of-a-kind))
  (assert (= (score [[10 :diamonds] [10 :spades] [:queen :hearts] [:queen :diamonds]
                     [10 :clubs]])
             :full-house))

  true)






;; don't need

(defn straight-ranks? [sorted-ranks]
  (or (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
      (= sorted-ranks '(14 5 4 3 2))))


(defn suit [card] (peek card))

(defn flush-hand? [hand]
  (apply = (map suit hand)))

