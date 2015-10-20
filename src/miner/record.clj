(ns miner.record
  (:require [clojure.string :as str]))

;;; CLJ-1105
;; defrecord classes implement IPersistentCollection but not .empty,
;; clojure.walk assumes collections support empty


;;; SEM -- checkout walk.clj -- It says it doesn't preserve maps from sorted-map-by, but it looks
;;; like maybe (empty smb) does the right thing and it actually works.

;; see also my "tagged" project

(defn record-class-factory [record-class]
  (let [rec-name (pr-str record-class)
        dot (.lastIndexOf rec-name ".")]
    (assert (pos? dot))
    (resolve (symbol (str (subs rec-name 0 dot) "/map->" (subs rec-name (inc dot)))))))

(defn map-factory-name [record-class]
  (let [cname (pr-str record-class)
        dot (.lastIndexOf cname ".")
        ns-name (subs cname 0 dot)
        rec-name (subs cname (inc dot))]
    (str ns-name "/map->" rec-name)))


;; walk adds...
#_   (instance form clojure.lang.IRecord)
#_      (outer ((record-map-factory-var (type form)) (into {} (map inner form))))


(defn empty-record [rec]
  (when-let [factory (record-class-factory (class rec))]
    (factory {})))

