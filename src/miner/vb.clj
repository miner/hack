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
  (let [frq (frequencies (concat (map :home results) (map :away results)))]
    (assoc frq 'total (reduce + (vals frq)))))

(defn wins [results]
  ;; seq or maps [(* {:ts long :winner int :home int :away int :time int})]
  (frequencies (map :winner results)))
                  
(defn report [results]
  (prn (freq results))
  (doseq [x (sort-by :ts results)]
    (prn x)))

(defmacro abba [test a b]
  `(let [a# ~a b# ~b]
     (if ~test [a# b#] [b# a#])))

(defn run-winners-court [label coff cowin colose cinhi cinlow cresult]
  (go (loop [home (<! coff) challenger (<! coff)]
        (when (and home challenger)
          (let [res (play-game label home challenger)]
            (>! cresult res)
            (let [[winner loser] (abba (= (:team home) (:winner res)) home challenger)]
              (>! colose loser)
              (>! cowin winner))
              (recur (<! cinhi) (<! cinlow)))))))

(defn run-gym-winners
  ([] (run-gym-winners 5 1000))
  ([nteams] (run-gym-winners nteams 1000))
  ([nteams msecs]
     (let [teams (map #(hash-map :team %) (range nteams))
           coff (chan (count teams))
           clwinner (chan 1)
           cwloser (chan 1)
           cwinner-stays (chan 1)
           cresult (chan)
           timer (timeout msecs)
           winners (run-winners-court "Winners" coff cwinner-stays cwloser cwinner-stays clwinner cresult)
           losers (run-winners-court  "Losers " coff clwinner coff cwloser coff cresult)]
       (<!! (go
             (doseq [team teams]
               (>! coff team))
             
             (loop [results []]
               (let [[res ch] (alts! [cresult timer])]
                 (condp = ch
                   timer results
                   cresult (if res (recur (conj results res)) results)))))))))


(defn run-court-round [label coff coh coa cih cia cresult]
  (go (loop [home (<! coff) away (<! coff)]
        (when (and home away)
          ;; play game and report result
          (>! cresult (play-game label home away))
          ;; home goes off
          (>! coh home)
          (>! coa away)
          (recur (<! cih) (<! cia))))))


(defn run-gym-round 
  ([] (run-gym-round 5 1000))
  ([nteams] (run-gym-round nteams 1000))
  ([nteams msecs]
     (let [teams (map #(hash-map :team %) (range nteams))
           cteams (chan (count teams))
           cresult (chan)
           cab (chan 1)
           cba (chan 1)
           caa (chan 1)
           timer (timeout msecs)
           court-a (run-court-round "A" cteams caa cab cba caa cresult)
           court-b (run-court-round "B" cteams cba cteams cteams cab cresult)]
       (<!! (go
             (doseq [team teams]
               (>! cteams team))
             
             (loop [results []]
               (let [[res ch] (alts! [cresult timer])]
                 (condp = ch
                   timer results
                   cresult (if res (recur (conj results res)) results)))))))))





(def sss (run-gym 7))
(def www (run-gym-winners 7))
(def rrr (run-gym-round 7))

