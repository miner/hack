(ns miner.hash63)


;;; idea: make perfect hash using 63 bits of long, 0 means not applicable so you need a real hash

;;; regular (hash x) => 32 bit Integer.  I want a 6 bit index so hash is single bit of long
;;; (max 63).  That would allow sets of hashes to be a single long with fast bit access
;;; operations.

;;; This seems feasible with a limited set of keys.  Keeping 0 as an escape hatch for out of
;;; band values.  Maybe find a bit field from hash codes that guarantees unique among those
;;; keys.  Might scan various hash schemes or bit widths to find unique fields for the
;;; limited key range.

;;; Maybe generate a case statement from results.

;;; Most unimplmented.  Probably a bad idea as someone else would have done it already if it
;;; made sense.

(defn hash63x
  ([i] (bit-and 63 (hash i)))
  ([i shift] (bit-and 63 (bit-shift-right (hash i) shift))))


(defn hash63slow
  ([i]
   (let [h (bit-and 63 (hash i))]
     (first (filter pos? [h (bit-and 63 i) 31]))))

  ([i shift]
   (let [h (bit-and 63 (bit-shift-right (hash i) shift))]
     (first (filter pos? [h (bit-and 63 i) 31])))) )


(defn hash63a
  ([i]
   (let [h (bit-and 63 (hash i))]
     (if (pos? h)
       h
       (let [b (bit-and 63 i)]
         (if (pos? b) b 31)))))

  ([i shift]
   (let [h (bit-and 63 (bit-shift-right (hash i) shift))]
     (if (pos? h)
       h
       (let [b (bit-and 63 i)]
         (if (pos? b) b 31))))))


(defn hash63 [i]
  (let [h (hash i)
        b (bit-and 63 (bit-xor h (bit-shift-right h 16)))]
     (if (pos? b)
       b
       (let [b (bit-and 63 i)]
         (if (pos? b) b 31)))))

;; do shift later
