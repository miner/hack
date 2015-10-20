
(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; A queue works "first in, first out."  You "conj" to the end (like a vector), and "pop"
;; from the head (like a list).

;; peek to see the head 
;; pop to pop off the head, returning new queue without head
;; conj to add to the tail
;; empty? to test for empty queue
;; seq to see the contents in order

;; last gives last element (tail)
;; butlast gives seq of all but last element


(defmethod print-method clojure.lang.PersistentQueue
  [q ^java.io.Writer w]
  (.write w "#queue ")
  (print-method (sequence q) w))

#_ (let [*data-readers* {'queue #'queue}]
     (read-string (pr-str (queue [1 2 3]))))

;; note that some my REPL didn't do the right thing with (set! *data-readers* ...)
;; probably a bug in REPL-y or nREPL


;; ----------------------------------------------------------------------
;; just playing around with subvec, not good code
;;   seriously, use rrb-vctor insead for better concat
;;
;; deque
;; conj at end last
;; peek -- get last (tail)
;; pop -- pops last
;; head -- (v 0)
;; pop head (subvec v 1)

;; really should use rrb-vector to get fast concat (conj-head)

(defn peek-head [v]
  (v 0))

(defn pop-head [v]
  (subvec 1))

(defn conj-head [v x]
  ;; not good
  (into [x] v))

