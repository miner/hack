(ns miner.test.core
  (:require [metrics.core :as met :refer [new-registry]]
            [metrics.timers :as tim])
  (:use [miner.core]
        [clojure.test]))



(defn report-msecs [msecs]
  (cond (< msecs 10000) (str msecs " msec")
        (< msecs (* 20 10000)) (str (/ (quot msecs 10) 100.0) " sec")
        (< msecs (* 20 60 10000)) (str (/ (quot msecs 600) 100.0) " min")
        :else (str msecs " msec")))

(defn report-nanosecs [nanos]
  (report-msecs (quot nanos 1e6)))


(deftest metricalish
  (let [reg (new-registry)
        timer (tim/timer reg "my-timer")]
    (is (not (nil? reg)))
    (tim/time! timer (Thread/sleep 2000))
    (tim/time! timer (Thread/sleep 1000))
    (tim/time! timer (Thread/sleep 1200))
    (tim/time! timer (Thread/sleep 1700))

    (println "my timer mean" (report-nanosecs (tim/mean timer)))

    (println "my timer smallest" (report-nanosecs (tim/smallest timer)))
    (println "my timer largest"  (report-nanosecs (tim/largest timer)))
    
    (println "my timer percentiles" (tim/percentiles timer))

    (is (> (tim/smallest timer) 800))
    (is (> (tim/largest timer) 1800))))


