(ns miner.scaffolding
  (:import (java.lang.reflect Method Parameter)))

;;; based on original from https://gist.github.com/semperos/3835392
;;; Modified by SEM to use Clojure 1.12 style interop

;;; Useful for prebuilding guts of a deftype based on a particular interface


;; Big thanks to Christophe Grand - https://groups.google.com/d/msg/clojure/L1GiqSyQVVg/m-WJogaqU8sJ

;; sadly, parameter names are not usually present

;;; SEM version
(defn scaffold [iface]
  (let [paramSymbol (fn [num param]
                      (if (Parameter/.isNamePresent param)
                        (symbol (Parameter/.getName param))
                        (or (nth '[a b c d e f g h i j k] num nil) (symbol (str "x" (inc num))))))]
    (doseq [[iface methods] (->> iface
                                 Class/.getMethods 
                                 (map #(vector (Class/.getName (Method/.getDeclaringClass %)) 
                                               (symbol (Method/.getName %))
                                               (into '[this]
                                                     (map-indexed paramSymbol)
                                                     (Method/.getParameters %))))
                                 (group-by first))]
      (println " " iface)
      (doseq [[_ name paramNames] methods]
        (println 
         (str "    " (list name paramNames)))))))
