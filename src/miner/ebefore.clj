(ns miner.ebefore
  (:require [clojure.string :as str]))


;;; https://gist.github.com/ericnormand/18a6858ea77c0a306bc75e4b3d7c5ce6

;;; Write a function that takes a string and two letters. The function should return true if
;;; every instance of the first letter comes before every instance of the second letter.
;;; Letters will be mixed case. You should treat \A the same as \a. You can assume the
;;; strings will contain instances of both letters.


#_
(require '[clojure.string :as str])

;; if c1 or c2 is not in s, return true (better than NPE)
(defn first-before? [s c1 c2]
  (let [s (str/lower-case s)
        ind2 (str/index-of s (str/lower-case c2))]
    (or (not ind2)
        (not (str/index-of s (str/lower-case c1) ind2)))))




(defn smoke-before
  ([] (smoke-before first-before?))
  ([first-before?]
   (assert (first-before? "A rabbit jumps joyfully" \a \j))
   (assert (not (first-before? "A jolly rabbit jumps joyfully" \a \j)))
   (assert (first-before? "Don't tread on me" \t \m))
   (assert (not (first-before? "Every React podcast" \t \d)))
   (assert (first-before? "Don't tread on me" \t \Z))
   (assert (first-before? "Don't tread on me" \Z \t))
   true))


;;; others, not as nice
(defn first-before3? [s c1 c2]
  (let [s (str/lower-case s)
        c1 (str/lower-case c1)
        c2 (str/lower-case c2)]
    (> (or (str/index-of s c2) 0)
       (or (str/last-index-of s c1) (count s)))))
  
(defn fbefore? [s c1 c2]
  (let [s (str/lower-case s)]
    (not (str/last-index-of s (str/lower-case c2) (str/last-index-of s (str/lower-case c1))))))

(defn first-before1? [s c1 c2]
  (let [s (str/lower-case s)]
    (not (str/index-of s (str/lower-case c1) (str/index-of s (str/lower-case c2))))))



;; steffan-westcott 
(defn first-before-sw? [s ch0 ch1]
  (-> (str "(?i)\\Q" ch1 "\\E.*\\Q" ch0 "\\E") re-pattern (re-find s) nil?))

;; nwjsmith 
(defn first-before-nw?
  [s first-letter second-letter]
  (->> (str/lower-case s)
       (drop-while (complement #{(Character/toLowerCase ^Character second-letter)}))
       (every? (complement #{(Character/toLowerCase ^Character first-letter)}))))
