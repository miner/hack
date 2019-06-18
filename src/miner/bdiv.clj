(ns miner.bdiv)



;; https://purelyfunctional.tv/issues/purelyfunctional-tv-newsletter-321-tip-do-not-count-sequences-from-strangers

;; binary divisible by 5
;;
;; Write a function that takes a string of binary numbers, separated by commas. It will look
;; like this: "1100,101,111,1111"


(require '[clojure.string :as str])

(defn bdiv5 [bstr]
  (->> (when-not (str/blank? bstr) (str/split bstr #","))
       (map #(Long/parseLong % 2))
       (filter #(zero? (rem % 5)))
       (map #(Long/toBinaryString %))
       (str/join ",")))


(defn smoke-bdiv []
  (let [bstr (fn [n] (Long/toBinaryString n))
        bparse (fn [s] (Long/parseLong s 2))
        s5x4 (str/join "," (map bstr (range 5 25 5)))
        s520  (str/join "," (map bstr (range 1 25)))]
    (assert (= (bdiv5 s5x4) s5x4))
    (assert (= (bdiv5 s520) s5x4))
    (assert (= (bdiv5 "") "")))
  true)

    
