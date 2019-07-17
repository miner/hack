(ns miner.poker
  (:require [clojure.string :as str]))

;; original:  https://rosettacode.org/wiki/Poker_hand_analyser#Clojure
;; hacked by SEM

;; I don't like some of the Rosetta requirements so mine is not necessarily a drop-in replacement.
;; Rosetta is overly string-based.  Clojure allows better data types.

;; The string representation for a card is a two-character or three-character string.
;; Faces are: a, 2, 3, 4, 5, 6, 7, 8, 9, 10, j, q, k
;; Suits are: h (hearts), d (diamonds), c (clubs), and s (spades), or alternatively the
;; unicode card-suit characters: ♥ ♦ ♣ ♠

;; My internal representation for a card is a two element vector of [rank suit] where rank
;; is a long 1 - 13 for Ace - King, and suit is a keyword :hearts, :diamonds :clubs, or :spades.


(def possible-evaluations
  [:straight-flush
 :four-of-a-kind
  :full-house
  :flush
  :straight
  :three-of-a-kind
  :two-pair
  :one-pair
  :high-card
  :invalid])

(defn parse-card [cstr]
  (let [rank (case (count cstr)
               2 (case (first cstr)
                   (\a \A \1) 1
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

(defn straight-hand? [hand]
  (let [ranks (sort (map first hand))]
    (or (= ranks (range (first ranks) (+ 5 (first ranks))))
        (and (= (first ranks) 1)
             (= (rest ranks) (range 10 14))))))

(defn flush-hand? [hand]
  (apply = (map second hand)))

(defn kind-max [hand]
  (let [freqs (frequencies (map first hand))]
    (apply max (vals freqs))))

(defn eval-hand [hand]
  (if hand
    (let [straight? (straight-hand? hand)
          flush? (flush-hand? hand)
          freqs (frequencies (map first hand))
          vfs (vals freqs)
          maxk (apply max vfs)]
      (cond (and straight? flush?) :straight-flush
            (= maxk 4) :four-of-a-kind
            (and (= maxk 3) (some #{2} vfs)) :full-house
            flush? :flush
            straight? :straight
            (= maxk 3) :three-of-a-kind
            (= (sort vfs) [1 2 2]) :two-pair
            (= maxk 2) :one-pair
            (= maxk 1) :high-card
            :else :invalid))
      :invalid))
          
(defn eval-hand-str [hand-str]
  (eval-hand (parse-hand hand-str)))

(defn test-hands []
  (doseq [h ["2H 2D 2S KS QD" "2H 5H 7D 8S 9D" "AH 2D 3S 4S 5S" "2H 3H 2D 3S 3D"
             "2H 7H 2D 3S 3D" "2H 7H 7D 7S 7C" "TH JH QH KH AH" "4H 4C KC 5D TC"
             "QC TC 7C 6C 4C"]]
    (println (str h ":") (name (eval-hand-str h)))))

          
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

#_ (run! println (map #(str % " : " (check-hand %)) hands))


