(ns miner.poker
  (:require [clojure.string :as str]))

;; original:  https://rosettacode.org/wiki/Poker_hand_analyser#Clojure
;; hacked by SEM

;; I don't like some of the Rosetta requirements so mine is not necessarily a drop-in replacement.
;; Rosetta is overly string-based.  Clojure allows better data types.

;; The string representation for a card is a two-character or three-character string.
;; Faces are: 2, 3, 4, 5, 6, 7, 8, 9, 10, j, q, k, a
;; Suits are: h (hearts), d (diamonds), c (clubs), and s (spades), or alternatively the
;; unicode card-suit characters: ♥ ♦ ♣ ♠

;; My internal representation for a card is a two element vector of [rank suit] where rank
;; is a long 2 - 14 (Ace high), and suit is a keyword :hearts, :diamonds :clubs, or :spades.


(def possible-evaluations
  [:invalid :high-card :one-pair :two-pair :three-of-a-kind :straight
   :flush :full-house :four-of-a-kind :straight-flush])

;; add five-of-a-kind with jokers


(def eval-map (reduce-kv (fn [r i v] (assoc r (* i 1000) v v (* i 1000))) {}
                         possible-evaluations))


(defn parse-card [cstr]
  (let [rank (case (count cstr)
               2 (case (first cstr)
                   (\a \A \1) 14
                   (\t \T) 10
                   (\j \J) 11
                   (\q \Q) 12
                   (\k \K) 13
                   (\2 \3 \4 \5 \6 \7 \8 \9) (- (long (first cstr)) (long \0))
                   nil)
               3 (when (str/starts-with? cstr "10") 10)
               nil)
        suit (case (last cstr)
               (\h \H \♥) :hearts
               (\d \D \♦) :diamonds
               (\c \C \♣) :clubs
               (\s \S \♠) :spades
               nil)]
    (when (and rank suit)
      [rank suit])))

(defn parse-hand [hand-str]
  (let [cards (mapv parse-card (str/split hand-str #"\W"))]
    (when (and (= (count cards) 5) (not-any? nil? cards) (apply distinct? cards))
      cards)))

(defn ace-low-straight? [sorted-ranks]
  (= sorted-ranks '(14 5 4 3 2)))

(defn simple-straight? [sorted-ranks]
  (= sorted-ranks (take 5 (iterate dec (first sorted-ranks)))))

(defn straight-ranks? [sorted-ranks]
  (or (simple-straight? sorted-ranks)
      (ace-low-straight? sorted-ranks)))  

(defn flush-hand? [hand]
  (apply = (map second hand)))


(defn score-hand [hand]
  (if hand
    (let [freqs (frequencies (map first hand))
          priorities (sort-by #(- (* -100 (val %)) (key %)) freqs)
          ranks (map key priorities)
          maxk (val (first priorities))
          stra? (straight-ranks? ranks)
          flush? (flush-hand? hand)
          evaluation (cond (and stra? flush?) :straight-flush
                           (= maxk 4) :four-of-a-kind
                           (= (map val priorities) [3 2]) :full-house
                           flush? :flush
                           stra? :straight
                           (= maxk 3) :three-of-a-kind
                           (= (map val priorities) [2 2 1]) :two-pair
                           (= maxk 2) :one-pair
                           (= maxk 1) :high-card
                           :else :invalid)]
      (if (and (= evaluation :straight) (ace-low-straight? ranks))
        (vec (conj (rest ranks) (eval-map :straight)))
        (vec (conj ranks (eval-map evaluation)))))
    [(eval-map :invalid)]))
          
(defn score-hand-str [hand-str]
  (score-hand (parse-hand hand-str)))

(defn test-hands []
  (doseq [h ["2H 2D 2S KS QD" "2H 5H 7D 8S 9D" "AH 2D 3S 4S 5S" "2H 3H 2D 3S 3D"
             "2H 7H 2D 3S 3D" "2H 7H 7D 7S 7C" "TH JH QH KH AH" "4H 4C KC 5D TC"
             "QC TC 7C 6C 4C"]]
    (let [score (score-hand-str h)]
    (println (str h ":") (name (eval-map (first score))) score))))

          
;; ----------------------------------------------------------------------
;; Original Rosetta Clojure example

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit ^Character fst)
      (Integer/valueOf (str fst))
      ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))
 
(defn suit [card]
  (let [[_ snd] card]
    (str snd)))
 
(defn n-of-a-kind [hand n]
  (not (empty? (filter #(= true %) (map #(>= % n) (vals (frequencies (map rank hand))))))))
 
(defn ranks-with-ace [hand]
  (let [ranks (sort (map rank hand))]
    (if (some #(= 14 %) ranks) (cons 1 ranks) ranks)))
 
(defn pair? [hand]
  (n-of-a-kind hand 2))
 
(defn three-of-a-kind? [hand]
  (n-of-a-kind hand 3))
 
(defn four-of-a-kind? [hand]
  (n-of-a-kind hand 4))
 
(defn flush? [hand]
  (not (empty? (filter #(= true %) (map #(>= % 5) (vals (frequencies (map suit hand))))))))
 
(defn full-house? [hand]
  (true? (and
    (some #(= 2 %) (vals (frequencies (map rank hand))))
    (some #(= 3 %) (vals (frequencies (map rank hand)))))))
 
(defn two-pairs? [hand]
  (or
    (full-house? hand)
    (four-of-a-kind? hand)
    (= 2 (count (filter #(= true %) (map #(>= % 2) (vals (frequencies (map rank hand)))))))))
 
(defn straight? [hand]
  (let [hand-a (ranks-with-ace hand)
        fst (first hand-a)
        snd (second hand-a)]
    (or
      (= (take 5 hand-a) (range fst (+ fst 5)))
      (= (drop 1 hand-a) (range snd (+ snd 5))))))
 
(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))
 
(defn invalid? [hand]
  (not= 5 (count (set hand))))
 
(defn check-hand [hand]
  (cond
    (invalid? hand) "invalid"
    (straight-flush? hand) "straight-flush"
    (four-of-a-kind? hand) "four-of-a-kind"
    (full-house? hand) "full-house"
    (flush? hand) "flush"
    (straight? hand) "straight"
    (three-of-a-kind? hand) "three-of-a-kind"
    (two-pairs? hand) "two-pair"
    (pair? hand) "one-pair"
    :else "high-card"))
 
; Test examples
(def hands [["2H" "2D" "2S" "KS" "QD"]
            ["2H" "5H" "7D" "8S" "9D"]
            ["AH" "2D" "3S" "4S" "5S"]
            ["2H" "3H" "2D" "3S" "3D"]
            ["2H" "7H" "2D" "3S" "3D"]
            ["2H" "7H" "7D" "7S" "7C"]
            ["TH" "JH" "QH" "KH" "AH"]
            ["4H" "4C" "KC" "5D" "TC"]
            ["QC" "TC" "7C" "6C" "4C"]])

(defn rosetta-test []
 (run! println (map #(str % " : " (check-hand %)) hands)))


