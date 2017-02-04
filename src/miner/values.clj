(ns miner.values)

;; SEM: I don't like the clojure.inspector/atom? definition.  Makes sense for Common Lisp,
;; but confusing for Clojure which has a different meaning for "atom" (as a reference type).

;; I suggest a new name for the old concept.

(defn scalar? [x]
  (not (coll? x)))
