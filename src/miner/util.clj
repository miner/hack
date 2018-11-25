(ns miner.util)

;; built-in `empty` throws on a record and returns nil on non-collections.  I'd prefer that
;; it return nil for records, too.  The empty map would also be OK with me, but I decided
;; it's not quite right to think of an empty record as an emtpy map because the record has
;; required keys.  The nil is a better indicator that something is missing, without having
;; to throw an exception.

;; other possible names: nugatory, emptiness, miner.util/empty
(defn zilch [x]
  (when-not (record? x)
    (empty x)))

    
  
