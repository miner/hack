(ns miner.vb
  (:require [clojure.core.async :as async
             :refer [<! >! chan close! put! alt! alts! timeout go <!! >!!]]))

;; fake for now
(defn play-game [court home away]
  ;(println "Court " court home "vs" away)
  (let [millis (System/currentTimeMillis)
        fake-time (+ 10 (rand-int 10))]
    (<!! (timeout fake-time))
    {:ts millis :court court :winner (if (zero? (rand-int 2)) (:team home) (:team away))
     :home (:team home) :away (:team away) :time fake-time }))

(defn run-court [court cteams cresult]
  (go (loop [home (<! cteams) away (<! cteams)]
        (when (and home away)
          ;; play game and report result
          (>! cresult (play-game court home away))
          ;; home goes off
          (>! cteams home) 
          ;; promote away to home and get a new away
          (recur away (<! cteams))))))

(defn run-gym 
  ([] (run-gym 20))
  ([max]
     (let [teams (map #(hash-map :team %) (range 7))
           cteams (chan (count teams))
           cresult (chan)
           ;; timer (timeout msecs)
           court-a (run-court "A" cteams cresult)
           court-b (run-court "B" cteams cresult)]
       (<!! (go
             (doseq [team teams]
               (>! cteams team))
             
             (loop [results []]
               (if (>= (count results) max)
                 results
                 (let [res (<! cresult)]
                   (if res (recur (conj results res)) results)))))))))

#_ (doseq [x (sort-by (juxt :court :ts) (vb/run-gym))] (prn x))





                  
        
        
