(ns miner.metered
  (:require (metrics [core :as met]
                     [counters :as mc]
                     [gauges :as mg]
                     [histograms :as mh]
                     [meters :as mm]
                     [timers :as mt]
                     [utils :as mu])))

(mc/defcounter sem-counter)

(defn test-counter []
  (println (mc/value sem-counter))
  (mc/inc! sem-counter)
  (println (mc/value sem-counter))
  (Thread/sleep 500)
  (println (mc/value sem-counter))
  (mc/inc! sem-counter)
  (mc/inc! sem-counter)
  (mc/inc! sem-counter)
  (println (mc/value sem-counter))
  (Thread/sleep 1500)
  (println (mc/value sem-counter))
  (mc/inc! sem-counter 10)
  (mc/inc! sem-counter)
  (println (mc/value sem-counter))
  (Thread/sleep 1500)
  (println (mc/value sem-counter)))


