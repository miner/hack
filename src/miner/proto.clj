;; http://stackoverflow.com/questions/4651756/how-do-i-find-implemented-protocols-in-clojure-object

;; Find all the implemented protocols of a value

(ns miner.proto
  (:require [clojure.string :as str]))


;; SEM: hack
;; inverse of resolve  
(defn var->symbol
  "Return symbol used to name Var v."
  [^clojure.lang.Var v]
  (when-let [sy (. v sym)]
    (symbol (name (ns-name (. v ns))) (name sy))))




(defn protocol? [maybe-p]
  (and (map? maybe-p)
       (every? #(contains? maybe-p %) [:on-interface :var])))

;; expensive, should cache
;; but how to invalidate?
(defn calc-all-protocol-vars []
  (keep (fn [vr] (when (protocol? (deref vr)) vr)) (mapcat vals (map ns-publics (all-ns)))))

(let [all-proto-vars-cache (delay (calc-all-protocol-vars))]
  (defn all-protocol-vars []
    (deref all-proto-vars-cache)))

(defn implemented-protocols [x]
  (map var->symbol (filter #(satisfies? (deref %) x) (all-protocol-vars))))




;; expensive resolve when class-not-found

(defn record-symbol? [sym]
  (when (simple-symbol? sym)
    (when-let [cl (try (resolve sym)
                       (catch ClassNotFoundException _ nil))]
      (and (class? cl)
           (.isAssignableFrom clojure.lang.IRecord cl )))))

;; faster
(defn record-sym? [sym]
  (when (simple-symbol? sym)
    (let [sss (str sym)
          dot (str/last-index-of sss ".")]
      (when (and dot )(> 0 dot (dec (.length sss)))
        (let [nsname (subs sss 0 dot)
              recname (subs (inc dot))
              factory (resolve (symbol nsname (str "map->" recname)))]
          (boolean factory))))))

;; maybe should memoize this for speed?
