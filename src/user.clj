(load-file "/Users/miner/Dropbox/clj/miner.clj")

;; temporary for working with Luhn credit card stuff
(use 'miner.luhn)

(require '[criterium.core :refer :all])

(def ccc (repeatedly 1000 #(gen-card 15)))

(def bbb (repeatedly 1000 #(gen-card 1234 12)))

(defn ben []
  (println "gara")
  (quick-bench (every? gara? ccc))
  (println)
  (println "check")
  (quick-bench (every? check? ccc))
  (println)
  (println "spec ::credit-card")
  (quick-bench (every? #(s/valid? :miner.luhn/credit-card %) ccc)))



(println "Loaded local src/user.clj")
