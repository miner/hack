(ns eric.rearrange
  (:require [clojure.string :as str]))


;; https://gist.github.com/ericnormand/a3489d47e163d84650a7c2f5ff32ecd6

;;; The words in your sentence have been mixed up. Luckily, there's a number embedded in
;;; each word that says its position in the sentence. Write a function that puts the words
;;; in the right order and removes the position digits.


;; assuming single digit
(defn rearrange [sentence]
  (->> (str/split sentence #" ")
       (reduce (fn [sm word] (assoc sm (re-find  #"\d" word) (str/replace word #"\d" "")))
               (sorted-map))
       vals
       (str/join " ")))

;; handle multiple consecutive digits
;; slower with read-string
;; new parse-long is basically same as Long/parseLong
(defn rearrange43 [sentence]
  (->> (str/split sentence #" ")
       (reduce (fn [sm word]
                 (assoc sm
                        (Long/parseLong (or (re-find  #"\d+" word) "0"))
                        (str/replace word #"\d+" "")))
               (sorted-map))
       vals
       (str/join " ")))



(defn num-word [word]
  [(re-find  #"\d" word) (str/replace word #"\d" "")])

(defn rearrange1 [sentence]
  (str/join " " (vals (into (sorted-map) (map num-word) (str/split sentence #" ")))))


(defn xrearrange [sentence]
  (transduce (map (fn [word] [(re-find  #"\d" word) (str/replace word #"\d" "")]))
             (completing conj #(str/join " " (vals %)))
             (sorted-map)
             (str/split sentence #" ")))


(defn smoke-re [rearrange]
  (assert (= (rearrange "World2! He1llo,") "Hello, World!"))
  (assert (= (rearrange "fo3r 5more Elegan1t 2weapons age.7 civil6ized a4")
             "Elegant weapons for a more civilized age."))
  (assert (= (rearrange "") ""))
  (assert (= (rearrange  "Th1is i2s a3 l4onger ex5ample tha6t t7akes 8multiple digits9 so10 i11t's 12a 13bit hard14er.")
    "This is a longer example that takes multiple digits so it's a bit harder."))
  true)


