(ns miner.epoker)

;; Eric's challenge: poker evaluation
;; https://gist.github.com/ericnormand/bf1408eb11d4c9a36a786cab946ef0c2

;; Cards are represented as a tuple of rank (number or name if it's a face card) and
;; suit. Face card names are the keywords :ace, :king, :queen, :jack. Suits
;; are :diamonds, :spades, :hearts, :clubs.



;; My internal representation for a card is a two element vector of [rank suit] where rank
;; is a long 2 - 14 (Ace high), and suit is a keyword :hearts, :diamonds :clubs, or :spades.


#_
(def numeric-scores (zipmap (range 100 1100 100)
                            [:high-card :pair :two-pair :three-of-a-kind :straight
                             :flush :full-house :four-of-a-kind :straight-flush :royal-flush]))

#_
(def valid-scores  (set (vals numeric-scores)))


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


(def srank (zipmap (range 2 15) [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace]))



(def symbolic-ranks [:jack :queen :king :ace])
(def symbolic-scores [:high-card :pair :two-pair :three-of-a-kind :straight
                      :flush :full-house :four-of-a-kind :straight-flush
                      :royal-flush])


(def isym (merge (zipmap (range 2 11) (range 2 11))
                 (zipmap (range 11 (+ 11 (count symbolic-ranks))) symbolic-ranks)
                 (zipmap symbolic-ranks (range 11 (+ 11 (count symbolic-ranks))))
                 (zipmap (range 20 (+ 20 (count symbolic-scores))) symbolic-scores)
                 (zipmap symbolic-scores (range 20 (+ 20 (count symbolic-scores))))))





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

;; slightly faster, but harder to read
(defn escore1 [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [nranks (map nrank hand)
        freqs (frequencies nranks)
        priorities (sort-by (fn [[nrk cnt]] (- (* -100 cnt) nrk)) freqs)
        sorted-ranks (map key priorities)
        flush? (apply = (map peek hand))]
    (if (and flush? (= sorted-ranks [14 13 12 11 10]))
      [:royal-flush]
      (let [low-ace-straight? (= sorted-ranks [14 5 4 3 2])
            regular-straight? (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))]
        (cond (and low-ace-straight? flush?) [:straight-flush (srank (second sorted-ranks))]
              (and regular-straight? flush?) [:straight-flush (srank (first sorted-ranks))]
              flush? (into [:flush] (map srank) sorted-ranks)
              low-ace-straight? [:straight (srank (second sorted-ranks))]
              regular-straight? [:straight (srank (first sorted-ranks))]
              :else (let [kind-counts (map val priorities)
                          max-kind (first kind-counts)
                          hand-kind (cond (= max-kind 4) :four-of-a-kind
                                          (= kind-counts [3 2]) :full-house
                                          (= max-kind 3) :three-of-a-kind
                                          (= kind-counts [2 2 1]) :two-pair
                                          (= max-kind 2) :pair
                                          :else :high-card)]
                      (into [hand-kind] (map srank) sorted-ranks)))))))


(defn escore [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [nranks (map nrank hand)
        priorities (sort-by (fn [[nrk cnt]] (- (* -100 cnt) nrk)) (frequencies nranks))
        sorted-ranks (map key priorities)
        flush? (apply = (map peek hand))
        low-ace-straight? (= sorted-ranks [14 5 4 3 2])
        regular-straight? (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))]
    (cond (and flush? (= sorted-ranks [14 13 12 11 10])) [:royal-flush]
          (and low-ace-straight? flush?) [:straight-flush (srank (second sorted-ranks))]
          (and regular-straight? flush?) [:straight-flush (srank (first sorted-ranks))]
          flush? (into [:flush] (map srank) sorted-ranks)
          low-ace-straight? [:straight (srank (second sorted-ranks))]
          regular-straight? [:straight (srank (first sorted-ranks))]
          :else (let [kind-counts (map val priorities)
                      max-kind (first kind-counts)
                      hand-kind (cond (= max-kind 4) :four-of-a-kind
                                      (= kind-counts [3 2]) :full-house
                                      (= max-kind 3) :three-of-a-kind
                                      (= kind-counts [2 2 1]) :two-pair
                                      (= max-kind 2) :pair
                                      :else :high-card)]
                  (into [hand-kind] (map srank) sorted-ranks)))))


;; lift let to top, maybe slower?

(defn escore2 [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [nranks (map nrank hand)
        priorities (sort-by (fn [[nrk cnt]] (- (* -100 cnt) nrk)) (frequencies nranks))
        sorted-ranks (map key priorities)
        flush? (apply = (map peek hand))
        low-ace-straight? (= sorted-ranks [14 5 4 3 2])
        regular-straight? (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
        kind-counts (map val priorities)
        max-kind (first kind-counts)]
    (cond (and flush? (= sorted-ranks [14 13 12 11 10])) [:royal-flush]
          (and low-ace-straight? flush?) [:straight-flush (srank (second sorted-ranks))]
          (and regular-straight? flush?) [:straight-flush (srank (first sorted-ranks))]
          flush? (into [:flush] (map srank) sorted-ranks)
          low-ace-straight? [:straight (srank (second sorted-ranks))]
          regular-straight? [:straight (srank (first sorted-ranks))]
          :else (into (cond (= max-kind 4) [:four-of-a-kind]
                            (= kind-counts [3 2]) [:full-house]
                            (= max-kind 3) [:three-of-a-kind]
                            (= kind-counts [2 2 1]) [:two-pair]
                            (= max-kind 2) [:pair]
                            :else [:high-card])
                      (map srank)
                      sorted-ranks))))




(defn vscore1 [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [nranks (map nrank hand)
        priorities (sort-by (fn [[nrk cnt]] (- (* -100 cnt) nrk)) (frequencies nranks))
        sorted-ranks (map key priorities)
        flush? (apply = (map peek hand))
        low-ace-straight? (= sorted-ranks [14 5 4 3 2])
        regular-straight? (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
        kind-counts (map val priorities)
        max-kind (first kind-counts)]
    (cond (and flush? (= sorted-ranks [14 13 12 11 10])) [(:royal-flush isym)]
                    (and low-ace-straight? flush?) [(:straight-flush isym) (second sorted-ranks)]
                    (and regular-straight? flush?) [(:straight-flush isym) (first sorted-ranks)]
                    flush? (into [(:flush isym)] sorted-ranks)
                    low-ace-straight? [(:straight isym) (second sorted-ranks)]
                    regular-straight? [(:straight isym) (first sorted-ranks)]
                    :else (into (cond (= max-kind 4) [(:four-of-a-kind isym)]
                                      (= kind-counts [3 2]) [(:full-house isym)]
                                      (= max-kind 3) [(:three-of-a-kind isym)]
                                      (= kind-counts [2 2 1]) [(:two-pair isym)]
                                      (= max-kind 2) [(:pair isym)]
                                      :else [(:high-card isym)])
                                sorted-ranks))))



(defn vscore [hand]
  ;;{:pre [(valid-hand? hand)]}
  (let [priorities (sort-by (fn [[nrk cnt]] (- (* -100 cnt) nrk))
                            (frequencies (map isym (map first hand))))
        sorted-ranks (map key priorities)
        flush? (apply = (map peek hand))
        low-ace-straight? (= sorted-ranks [14 5 4 3 2])
        regular-straight? (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
        kind-counts (map val priorities)
        max-kind (first kind-counts)]
    (cond (and flush? (= sorted-ranks [14 13 12 11 10])) [(:royal-flush isym)]
                    (and low-ace-straight? flush?) [(:straight-flush isym) (second sorted-ranks)]
                    (and regular-straight? flush?) [(:straight-flush isym) (first sorted-ranks)]
                    flush? (into [(:flush isym)] sorted-ranks)
                    low-ace-straight? [(:straight isym) (second sorted-ranks)]
                    regular-straight? [(:straight isym) (first sorted-ranks)]
                    :else (into (cond (= max-kind 4) [(:four-of-a-kind isym)]
                                      (= kind-counts [3 2]) [(:full-house isym)]
                                      (= max-kind 3) [(:three-of-a-kind isym)]
                                      (= kind-counts [2 2 1]) [(:two-pair isym)]
                                      (= max-kind 2) [(:pair isym)]
                                      :else [(:high-card isym)])
                                sorted-ranks))))




;; orig
(defn orig-enc6b [v]
  (reduce-kv (fn [s i b] (+ s (bit-shift-left b (* 8 (- 5 i))))) 0 v))

(defn improved-encode6b [v]
  (reduce-kv (fn [s i b] (bit-or s (bit-shift-left b (- 40 (bit-shift-left i 3))))) 0 v))

;;; bit-or about the same as unchecked-add


;; 6 bytes (255) encoded into one long
;; unrolled is twice as fast
(defn encode6b [v]
  (bit-or (bit-shift-left (nth v 0 0) 40)
          (bit-shift-left (nth v 1 0) 32)
          (bit-shift-left (nth v 2 0) 24)
          (bit-shift-left (nth v 3 0) 16)
          (bit-shift-left (nth v 4 0) 8)
          (nth v 5 0)))

;; SEM:  write a macro to do unrolling
;; bindings must have compile-time constant seqs
;; see hack/miner/unroll.clj







(defn nscore [hand]
  (encode6b (vscore hand)))


(defn decode6b [^long n]
  (loop [ds () r n]
    (if (zero? r)
      (into [] (take-while pos?) ds)
      (recur (conj ds (bit-and r 0xFF)) (unsigned-bit-shift-right r 8)))))






(defn decode-vec [v]
  (mapv isym v))

(defn enscore [hand]
  (decode-vec (vscore hand)))

(defn decode-score [n]
  (decode-vec (decode6b n)))

(defn winning-hand [a b]
  (if (> (nscore a) (nscore b))
    a
    b))


(defn compare-hands [a b]
  (compare (nscore a) (nscore b)))

(defn winhand [a b]
  (let [c (compare-hands a b)]
    (cond (zero? c) (list :Tie (enscore a))
          (pos? c) (list :First (enscore a) :beats (enscore b))
          :else (list :Second (enscore b) :beats (enscore a)))))







;; FIXED BUG in old xscore (deleted) -- need to have final score cards sorted by freq + rank 
;; use the card ranks to make an extended score that can properly `compare` to others

;; FIXED BUG:  if it's a low ace in a straight, it's a 1 not a 14.  Gotta special case
;; that.



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



(defn smoke-nscores []
  (let [full-ten-queen
        [[10 :diamonds] [10 :spades] [:queen :hearts] [:queen :diamonds] [10 :clubs]]
        full-queen-ten
        [[10 :diamonds] [:queen :spades] [:queen :hearts] [:queen :diamonds] [10 :clubs]]]
    (assert (= (winning-hand full-queen-ten full-ten-queen) full-queen-ten))
    (assert (= (winning-hand full-ten-queen full-queen-ten) full-queen-ten)))
  true)










;; don't need

(defn straight-ranks? [sorted-ranks]
  (or (= sorted-ranks (take 5 (iterate dec (first sorted-ranks))))
      (= sorted-ranks '(14 5 4 3 2))))


(defn suit [card] (peek card))

(defn flush-hand? [hand]
  (apply = (map suit hand)))






;;; At first I was using nibbles (4 bits) but decided to change to bytes (8 bits) to fit
;;; scores and cards into one encoding map

(defn encode6hex [coll]
  (reduce (fn [s n] (+ (* 16 s) n)) 0 (take 6 (concat coll (repeat 0)))))



(defn encode6h [coll]
  (reduce (fn [s n] (+ (bit-shift-left s 4) n)) 0 (take 6 (concat coll (repeat 0)))))

(defn enc6v [v]
  (reduce-kv (fn [s i b] (bit-or s (bit-shift-left b (* 4 (- 5 i))))) 0 v))

;; twice as fast!
(defn enc6vv [v]
  (reduce-kv (fn [s i b] (bit-or s (bit-shift-left b (- 20 (bit-shift-left i 2))))) 0 v))



;; not faster
(defn enc6s [v]
  (reduce + 0 (map bit-shift-left v [20 16 12 8 4 0])))


(defn enc6p [v]
  (loop [v v sh [20 16 12 8 4 0] sum 0]
    (if-let [b (peek v)]
      (recur (pop v) (pop sh) (bit-or sum (bit-shift-left b (peek sh))))
      sum)))

;; twice as fast!
(defn encode6v [v]
  (reduce-kv (fn [s i b] (bit-or s (bit-shift-left b (- 20 (bit-shift-left i 2))))) 0 v))

;; not really faster, but uglier
(defn dec6b [^long n]
  (loop [ds () r n]
    (if (zero? r)
      (reduce conj [] ds)
      (let [b (bit-and r 0xFF)
            r2 (unsigned-bit-shift-right r 8)]
        (if (zero? b)
          (recur ds r2)
          (recur (conj ds b) r2))))))


(defn decode6hex [n]
  (loop [ds () r n]
    (if (= (count ds) 6)
      (vec ds)
      (recur (conj ds (mod r 16)) (quot r 16)))))


(defn decode6h [^long n]
  (loop [ds () r n]
    (if (= (count ds) 6)
      (vec ds)
      (recur (conj ds (bit-and r 0xF)) (bit-shift-right r 4)))))


;;; NO, too slow
(defn BADun6b [v]
  (case (count v)
    0 0
    1 (bit-shift-left (nth v 0) 40)
    2 (bit-or (bit-shift-left (nth v 0) 40)
              (bit-shift-left (nth v 1) 32))
    3 (bit-or (bit-shift-left (nth v 0) 40)
              (bit-shift-left (nth v 1) 32)
              (bit-shift-left (nth v 2) 24))
    4 (bit-or (bit-shift-left (nth v 0) 40)
              (bit-shift-left (nth v 1) 32)
              (bit-shift-left (nth v 2) 24)
              (bit-shift-left (nth v 3) 16))
    5 (bit-or (bit-shift-left (nth v 0) 40)
              (bit-shift-left (nth v 1) 32)
              (bit-shift-left (nth v 2) 24)
              (bit-shift-left (nth v 3) 16)
              (bit-shift-left (nth v 4) 8))
    6 (bit-or (bit-shift-left (nth v 0) 40)
              (bit-shift-left (nth v 1) 32)
              (bit-shift-left (nth v 2) 24)
              (bit-shift-left (nth v 3) 16)
              (bit-shift-left (nth v 4) 8)
              (nth v 5))))

