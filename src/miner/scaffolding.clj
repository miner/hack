(ns miner.scaffolding
  (:import (java.lang.reflect Method Parameter)))

;;; based on original from  https://gist.github.com/semperos/3835392
;;; Big thanks to Christophe Grand
;;; - https://groups.google.com/d/msg/clojure/L1GiqSyQVVg/m-WJogaqU8sJ

;;; Useful for prebuilding guts of a deftype or defrecord based on a particular interface.
;;; Extended by SEM to take any sample object and tries to use its base class and supers to
;;; find relevant interfaces.  Might be overkill but it's easy enough to delete stuff you
;;; don't want.  Of course, the "xxx" in the stubs represents code you still have to write.
;;; And you might want to change the names of the parameters.  Sadly, the parameter names
;;; are not usually present in the Java runtime.

;;; Modified by SEM to use Clojure 1.12 style interop.  Refactored to support flexible input
;;; so it accepts and interface, class, or sample object.

(defn imethods [iface]
  (let [paramSymbol (fn [num param]
                      (if (Parameter/.isNamePresent param)
                        (symbol (Parameter/.getName param))
                        (or (nth '[a b c d e f g h i j k] num nil) (symbol (str "x" (inc num))))))]
    (->> iface
         Class/.getMethods 
         (map #(vector (Class/.getName (Method/.getDeclaringClass %)) 
                       (symbol (Method/.getName %))
                       (into '[this]
                             (map-indexed paramSymbol)
                             (Method/.getParameters %)))))))

;;; Some interfaces can be inherited multiple ways so we sort and dedupe the raw method
;;; info.

;;; flexible input, accepts interface, class or object
(defn scaffolding [class-or-obj]
  (when class-or-obj
    (let [c (if (class? class-or-obj) class-or-obj (class class-or-obj))
          iface? (Class/.isInterface c)
          sups (when-not iface? (supers c))
          isups (if iface? (list c) (filter Class/.isInterface sups))
          meths (dedupe (sort (mapcat imethods isups)))]
      (when-not iface? (println ";Stubs for" c))
      (doseq [[iface methods] (group-by first meths)]
        (println " " iface)
        (doseq [[_ name paramNames] methods]
          (println 
           (str "    " (list name paramNames 'xxx)))))
      (when-not iface?
        (doseq [d (remove Class/.isInterface sups)]
            ;; notes concrete superclass
            (println "; super" d))))))
