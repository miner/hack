(ns miner.rps
  (:require [clojure.core.async :refer :all]))

;; original: https://gist.github.com/puredanger/5965883

(def MOVES [:rock :paper :scissors])
(def BEATS {:rock :scissors, :paper :rock, :scissors :paper})

(defn rand-player
  "Create a named player and return a channel to report moves."
  [name]
  (let [out (chan)]
    (go (while true (>! out [name (rand-nth MOVES)])))
    out))

(defn winner
  "Based on two moves, return the name of the winner."
  [[name1 move1] [name2 move2]]
  (cond
   (= move1 move2) "no one"
   (= move2 (BEATS move1)) name1
   :else name2))

(defn judge
  "Given two channels on which players report moves, create and return an
   output channel to report the results of each match as [move1 move2 winner]."
  [p1 p2]
  (let [out (chan)]
    (go
     (while true
       (let [m1 (<! p1)
             m2 (<! p2)]
         (>! out [m1 m2 (winner m1 m2)]))))
    out))

(defn init
  "Create 2 players (by default Alice and Bob) and return an output channel of match results."
  ([] (init "Alice" "Bob"))
  ([n1 n2] (judge (rand-player n1) (rand-player n2))))

(defn report
  "Report results of a match to the console."
  [[name1 move1] [name2 move2] winner]
  (println)
  (println name1 "throws" move1)
  (println name2 "throws" move2)
  (println winner "wins!"))

(defn play
  "Play by taking a match reporting channel and reporting the results of the latest match."
  [out-chan]
  (apply report (<!! out-chan)))

(defn play-many
  "Play n matches from out-chan and report a summary of the results."
  [out-chan n]
  (loop [remaining n
         results {}]
    (if (zero? remaining)
      results
      (let [[m1 m2 winner] (<!! out-chan)]
        (recur (dec remaining)
               (merge-with + results {winner 1}))))))

;; SEM
(defn sim
  ([] (play-many (init) 1000))
  ([n] (play-many (init) n))
  ([n name1 name2] (play-many (init name1 name2) n)))
