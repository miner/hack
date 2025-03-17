(ns miner.deque)

;;; https://dev.to/borba/clojure-is-awesome-part-15-ac7

;;; hacked a bit by SEM for my testing.  Removed spec.
;;; old (ns clojure-is-awesome.deque)

(defprotocol Deque
  "A protocol defining double-ended queue operations."
  (push-front [this element]
    "Adds an element to the front. Returns a new deque.")
  (push-back [this element]
    "Adds an element to the back. Returns a new deque.")
  (pop-front [this]
    "Removes and returns the front element and new deque as a map {:element, :deque}.")
  (pop-back [this]
    "Removes and returns the back element and new deque as a map {:element, :deque}.")
  (peek-front [this]
    "Returns the front element without removing it, or nil if empty.")
  (peek-back [this]
    "Returns the back element without removing it, or nil if empty.")
  (deque-size [this]
    "Returns the number of elements in the deque."))

;; Record Definition
(defrecord VectorDeque [elements]
  Deque
  (push-front [_ element]
    (VectorDeque. (into [element] elements)))

  (push-back [_ element]
    (VectorDeque. (conj elements element)))

  (pop-front [_]
    (if (empty? elements)
      {:element nil :deque (VectorDeque. elements)}
      {:element (first elements) :deque (VectorDeque. (vec (rest elements)))}))

  (pop-back [_]
    (if (empty? elements)
      {:element nil :deque (VectorDeque. elements)}
      {:element (peek elements) :deque (VectorDeque. (pop elements))}))

  (peek-front [_]
    (first elements))

  (peek-back [_]
    (peek elements))

  (deque-size [_]
    (count elements)))

(defn create-deque
  "Creates an empty deque implemented as a VectorDeque.
   Returns:
     A new VectorDeque instance."
  []
  (VectorDeque. []))

(comment
(s/def ::elements vector?)
(s/def ::deque (s/keys :req-un [::elements]))
(s/def ::element any?)
)

(comment
  
(def my-deque (-> (create-deque)
                  (push-back 1)
                  (push-back 2)
                  (push-front 0)))

(println "Deque:" (:elements my-deque))    ;; Deque: [0 1 2]
(println "Front:" (peek-front my-deque))   ;; Front: 0
(println "Back:" (peek-back my-deque))     ;; Back: 2
(println "Size:" (deque-size my-deque))    ;; Size: 3

(def front-result (pop-front my-deque))
(println "Popped front:" (:element front-result)) ;; Popped front: 0
(println "New deque:" (:elements (:deque front-result))) ;; New deque: [1 2]

)


;;; SEM: why do that hybrid map/record?  If you have a record, always use it and return
;;; another record.  OK, it looks like I misunderstood the pop-front and pop-back which
;;; return stylized maps -- I think they should just pop and return the new Deque.  Use
;;; peek* first if you want the element.

;;; In this case, it's probably easier to use a canonical map.  My idea would be to use
;;; distinquished namespaced keys.  Assuming no hostile data, this should be a reasonable
;;; trade-off.  You could make it a record if you want a slight performance improvement.  It
;;; would also be better for participating in some Clojure protocols.
;;;
;;; Not saying this is a good idea.  A serious implementation would be more like
;;; Clojure's internal PersistentQueue.java.  Or maybe use rrb-vector and get fast concat.


;;; ::front is always a list, ::rear is always a vector
;;; constructor
(defn dq
  ([coll]
   (if (vector? coll)
     {::front nil ::rear coll}
     {::front (seq coll) ::rear []}))
  ([front rear]
   {::front (seq front) ::rear (vec rear)}))
  
(defn dq? [dq]
  (and (map? dq)
       (contains? dq ::front)
       (contains? dq ::rear)))

(defn dq-empty? [dq]
  (and (empty? (::front dq)) (empty? (::rear dq))))

(defn dq-count [dq]
  (+ (count (::front dq)) (count (::rear dq))))

(defn dq-pop-front [dq]
  (if-let [fr (seq (::front dq))]
    (assoc dq ::front (rest fr))
    (update dq ::rear subvec 1)))

(defn dq-pop-back [dq]
  (let [rr (::rear dq)]
    (if (pos? (count rr))
      (assoc dq ::rear (pop rr))
      {::front nil ::rear (pop (vec (::front dq)))})))

(defn dq-peek-front [dq]
  (if-let [fr (seq (::front dq))]
    (first fr)
    (nth (::rear dq) 0 nil)))

(defn dq-peek-back [dq]
  (let [rr (::rear dq)]
    (if (pos? (count rr))
      (peek rr)
      (last (::front dq)))))

(defn dq-push-front [dq x]
  (update dq ::front conj x))

(defn dq-push-back [dq x]
  (update dq ::rear conj x))

(defn dq-seq [dq]
  (concat (::front dq) (::rear dq)))
