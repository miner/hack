(ns miner.scaffolding
  (:import (java.lang.reflect Method Parameter)))

;;; based on original from https://gist.github.com/semperos/3835392
; Big thanks to Christophe Grand - https://groups.google.com/d/msg/clojure/L1GiqSyQVVg/m-WJogaqU8sJ

;;; Useful for prebuilding guts of a deftype or defrecord based on a particular interface.
;;; Extended by SEM to take any sample object and tries to use its base class and supers to
;;; find relevant interfaces.  Might be overkill but it's easy enough to delete stuff you
;;; don't want.  Of course, the "xxx" in the stubs represents code you still have to write.
;;; And you might want to change the names of the parameters.  Sadly, the parameter names
;;; are not usually present in the Java runtime.

;;; Modified by SEM to use Clojure 1.12 style interop

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
         (str "    " (list name paramNames 'xxx)))))))

;;; flexible input, accepts interface, class or object
(defn scaffolding [class-or-obj]
  (when class-or-obj
    (let [c (if (class? class-or-obj) class-or-obj (class class-or-obj))]
      (if (Class/.isInterface c)
        (scaffold c)
        (do
          (println "Stubs for" c)
          (doseq [iface (filter Class/.isInterface (supers c))]
            (scaffold iface))
          (println)
          (doseq [c (remove Class/.isInterface (supers c))]
            ;; notes concrete superclass
            (println "; super" c)))))))




