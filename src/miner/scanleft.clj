(ns miner.scanleft
  (:require [net.cgrand.xforms :as x]))

;;; from email on Clojure list by Jules

;;; [looking for a good Clojure way to do this...]
;;; It is a cross between 'map', 'reduce' and 'iterate'...
;;; Given a function 'f' and a sequence 's' it would return you  a sequence of :
;;; [(f s[0]) (f (f s[0]) s[1]) (f (f (f s[0]) s[1]) s[2]) ...]
;;;
;;;  (stateful-map + [0 1 2 3 4 5]) ==> [0 1 3 6 10 15]



(defn stateful-map-1 [f [h & t]]
  (reduce
   (fn [acc v]
     (conj acc (f (last acc) v)))
   [h]
   t))


;;; Sam Richie suggested `reductions` or looking for what other languages call scan-left.
;;; SEM: reductions seems exactly right to me.

;;; from email on Clojure list by Peter Stromberg, after ChatGPT.
;;; However, this is buggy, especially for the reducer version.

(defn buggy-scan-left-2
  ([f xs]
   (let [s (reductions f xs)]
     (butlast s)))
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (let [s (reductions f input)]
          (rf result (butlast s))))))))

(defn test-sl [scan-left]
  (assert (= (scan-left + [0 1 2 3 4 5]) '(0 1 3 6 10 15)))
  (assert (= (scan-left + [10 20 30]) '(10 30 60)))
  ;;(assert (= (into [] (scan-left +) '(0 1 2 3 4 5)) [0 1 3 6 10 15]))
  true)

(defn xscan-left
  ([f coll] (sequence (xscan-left f) coll))
  ([f] (comp (x/reductions f) (drop 1))))

