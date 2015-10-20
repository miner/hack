(ns miner.vb
  (:require [clojure.core.async :as async
             :refer [<! >! chan close! put! alt! alts! timeout go <!! >!!]]))

;; fake for now
(defn play-game [court home away]
  ;(println "Court " court home "vs" away)
  (let [millis (System/currentTimeMillis)
        fake-time (+ 25 (rand-int 50))]
    (<!! (timeout fake-time))
    (let [high (max (:team home) (:team away))
          low (min (:team home) (:team away))
          ;; biased towards low numbered teams
          winner (if (< (rand-int (+ high low)) high) low high)]
      {:ts millis :court court :winner winner
       :home (:team home) :away (:team away) :time fake-time })))

(defn run-court [label cteams cresult]
  (go (loop [home (<! cteams) away (<! cteams)]
        (when (and home away)
          ;; play game and report result
          (>! cresult (play-game label home away))
          ;; home goes off
          (>! cteams home) 
          ;; promote away to home and get a new away
          (recur away (<! cteams))))))

(defn run-gym 
  ([] (run-gym 5 1000))
  ([nteams] (run-gym nteams 1000))
  ([nteams msecs]
     (let [teams (map #(hash-map :team %) (range nteams))
           cteams (chan (count teams))
           cresult (chan)
           timer (timeout msecs)
           court-a (run-court "A" cteams cresult)
           court-b (run-court "B" cteams cresult)]
       (<!! (go
             (doseq [team teams]
               (>! cteams team))
             
             (loop [results []]
               (let [[res ch] (alts! [cresult timer])]
                 (condp = ch
                   timer results
                   cresult (if res (recur (conj results res)) results)))))))))



(defn freq [results]
  ;; seq or maps [(* {:ts long :winner int :home int :away int :time int})]
  (frequencies (concat (map :home results) (map :away results))))

(defn wins [results]
  ;; seq or maps [(* {:ts long :winner int :home int :away int :time int})]
  (frequencies (map :winner results)))
                  
(defn report [results]
  (let [fr (freq results)]
    (pr fr) 
    (println "  total " (reduce + (vals fr))))
  (doseq [x (sort-by :ts results)]
    (prn x)))


        
        
(defn run-winners [label coff cincoming coutgoing cresult]
  (go (loop [champ (<! coff) challenger (<! coff)]
        (when (and champ challenger)
          (let [res (play-game label champ challenger)]
            (>! cresult res)
            (let [defends? (= (:team champ) (:winner res))
                  winner (if defends? champ challenger)
                  loser (if defends? challenger champ)]
              (>! coutgoing loser)
              (recur winner (<! cincoming))))))))

(defn run-losers [label coff cincoming coutgoing cresult]
  (go (loop [home (<! coff) challenger (<! coff)]
        (when (and home challenger)
          (let [res (play-game label home challenger)]
            (>! cresult res)
            (let [home? (= (:team home) (:winner res))
                  winner (if home? home challenger)
                  loser (if home? challenger home)]
              (>! coff loser)
              (>! coutgoing winner)
              (recur (<! cincoming) (<! coff))))))))


(defn run-gym-winners
  ([] (run-gym-winners 5 1000))
  ([nteams] (run-gym-winners nteams 1000))
  ([nteams msecs]
     (let [teams (map #(hash-map :team %) (range nteams))
           coff (chan (count teams))
           cwinner (chan 2)
           closer (chan 2)
           cresult (chan)
           timer (timeout msecs)
           winners (run-winners "Winners" coff cwinner closer cresult)
           losers (run-losers  "Losers " coff closer cwinner cresult)]
       (<!! (go
             (doseq [team teams]
               (>! coff team))
             
             (loop [results []]
               (let [[res ch] (alts! [cresult timer])]
                 (condp = ch
                   timer results
                   cresult (if res (recur (conj results res)) results)))))))))

(def rrr (run-gym 7))
(def www (run-gym-winners 7))
