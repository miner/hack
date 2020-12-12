;; just code, not namespace

;; calls (f init) then f on the nested result `n` times.  Zero-th is just init.
;; condensed loop
(defn iterated [n f init]
  (if (pos? n)
    (recur (unchecked-dec n) f (f init))
    init))


;; natural way, somewhat slower
(defn iterated-naturally [n f init]
  (nth (iterate f init) n))

;; about the same speed, a bit obscure
(defn iterated-RED [n f init]
  (reduce (fn [r _] (f r)) init (range n)))

;;; original loop before I noticed how to save a line
(defn iterated-LOOP [n f init]
  (loop [z n res init]
    (if (pos? z)
      (recur (unchecked-dec z) (f res))
      res)))

;; not as fast, but might
(defn iterated-lazy [n f init]
  (first (sequence (drop n) (iterate f init))))


#_
(iterated 10 inc 3)
;;=> 13
#_
(iterated 0 inc 3)
;;=> 3

;; notice how builtin `iterate` returns init first
#_
(take 10 (iterate inc 3))
;;=> (3 4 5 6 7 8 9 10 11 12)
