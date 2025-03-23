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


;;; SEM: At first, I misunderstood the pop-front and pop-back which
;;; return stylized maps for multiple values.  I think they should just pop and return the
;;; new Deque.  Use peek* first if you want the element.

;;; In this case, it's probably easier to use a canonical map.  My idea would be to use
;;; distinquished namespaced keys.  Assuming no hostile data, this should be a reasonable
;;; trade-off.  You could make it a record if you want a slight performance improvement.  It
;;; would also be better for participating in some Clojure protocols.
;;;
;;; Not saying this is a good idea.  A serious implementation would be more like
;;; Clojure's internal PersistentQueue.java.  Or maybe use rrb-vector and get fast concat.

;;; Thinking a bit more, it would be a better example to make a record with the normal
;;; Clojure collection protocols implemented.  Maybe add a bit more to allow peek-, pop-,
;;; and conj-front.  A deftype would be more proper as we don't need map-sytle access, but
;;; it's convenient for the implementation, at least for my current rdq code which is
;;; map-based.


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


(comment  All the supers of a vector   (supers (class [1]))
  clojure.lang.IObj
  clojure.lang.IKVReduce
  clojure.lang.AFn
  clojure.lang.IReduce
  java.util.concurrent.Callable
  clojure.lang.IPersistentCollection
  java.lang.Iterable
  java.util.SequencedCollection
  clojure.lang.Seqable
  clojure.lang.IPersistentVector
  clojure.lang.Indexed
  clojure.lang.Counted
  clojure.lang.IEditableCollection
  clojure.lang.Sequential
  clojure.lang.IHashEq
  clojure.lang.IPersistentStack
  java.lang.Object
  clojure.lang.IReduceInit
  java.io.Serializable
  java.lang.Comparable
  clojure.lang.APersistentVector
  clojure.lang.ILookup
  java.util.Collection
  java.util.RandomAccess
  java.util.List
  clojure.lang.Associative
  clojure.lang.IMeta
  clojure.lang.IDrop
  clojure.lang.Reversible
  java.lang.Runnable
  clojure.lang.IFn
)




;;; new idea -- keep two vectors with front considered reversed, rear normal vector

;;; make it a record so it can participate in the usual collection protocols
;;; treat like a vector but add something like a reverse stack pop, push, peek at front
;;; maybe a new protocol for front access

(defn rdq [coll]
  {::rfront [] ::v (vec coll)})
  
(defn rdq? [rdq]
  (and (map? rdq)
       (contains? rdq ::rfront)
       (contains? rdq ::v)))

(defn rdq-empty? [dq]
  (and (zero? (count (::v rdq))) (zero? (count (::rfront rdq)))))

(defn rdq-count [rdq]
  (+ (count (::rfront rdq)) (count (::v rdq))))

(defn rdq-pop-front [rdq]
  (if (pos? (count (::rfront rdq)))
    (update rdq ::rfront pop)
    (update rdq ::v subvec 1)))

(defn rdq-pop-back [rdq]
  (if (pos? (count (::v rdq)))
    (update rdq ::v pop)
	(update rdq ::rfront subvec 1)))


(defn rdq-peek-front [rdq]
  (if (pos? (count (::rfront rdq)))
	(peek (::rfront rdq))
	(nth (::v rdq) 0 nil)))


(defn rdq-peek-back [dq]
    (if (pos? (count (::v dq)))
      (peek (::v dq))
	  (nth (::rfront rdq) 0 nil)))

(defn rdq-push-front [rdq x]
  (update rdq ::rfront conj x))

(defn rdq-push-back [rdq x]
  (update rdq ::v conj x))

(defn rdq-seq [rdq]
  (concat (rseq (::rfront rdq)) (::v rdq)))


(deftype Deq [rfront v]
  clojure.lang.IFn
    (applyTo [this args] (assert (= (count args) 1)) (nth this (first args)))
    (invoke [this n] (.nth this n))

  clojure.lang.IPersistentCollection
    (cons [this a] (Deq. rfront (conj v a)))
    ;;(count [this] duplicate)
    (empty [this] (Deq. [] []))
    (equiv [this x] (or (identical? this x) (= (seq this) (seq x))))

  clojure.lang.IKVReduce
  (kvreduce [this f init]
    (let [rfcnt (count rfront)]
      (reduce-kv (fn [res k x] (f res (+ k rfcnt) x))
                 (reduce (fn [r i] (f r i (rfront (- (dec rfcnt) i))))
                         init
                         (range rfcnt))
                 v)))

 clojure.lang.Counted
  (count [this] (+ (count rfront) (count v)))

  clojure.lang.IReduce
    (reduce [this f] (reduce f (concat (rseq rfront) v)))

  clojure.lang.IHashEq
     (hasheq [this] (hash (seq this)))

  ;;clojure.lang.IEditableCollection
  ;;  (asTransient [this] xxx)

  clojure.lang.Seqable
    (seq [this] (concat (rseq rfront) v))

  clojure.lang.IPersistentVector
  (assocN [this n x]
    (let [rfcnt (count rfront)]
      (if (< n rfcnt)
        (Deq. (assoc rfront (- (dec rfcnt) n) x) v)
        (Deq. rfront (assoc v (- n rfcnt) x)))))

    ;; wrong end anyway (cons [this a] (assoc :rfront conj a))
    (length [this] (+ (count rfront) (count v)))

  java.lang.Comparable
    (compareTo [this x] (compare (concat (rseq rfront) v) (seq x)))

  clojure.lang.IReduceInit
    (reduce [this f init] (reduce f init (concat (rseq rfront) v)))

  clojure.lang.Indexed
  (nth [this n]
    (let [fcnt (count rfront)]
      (if (< n fcnt)
        (nth rfront (- (dec fcnt) n))
        (nth v (- n fcnt)))))
  (nth [this n default]
    (if (< n (count this))
      (nth this n)
      default))

  clojure.lang.IDrop
  (drop [this n]
    (let [rfcnt (count rfront)]
      (if (< n rfcnt)
        (Deq. (subvec rfront 0 (- rfcnt n)) v)
        (if (< n (count this))
          (Deq. [] (subvec v (- n rfcnt)))
          (Deq. [] [])))))

  clojure.lang.Associative
  (assoc [this n x]
    (let [fcnt (count rfront)]
      (if (< n fcnt)
        (Deq. (assoc rfront (- (dec fcnt) n) x) v)

****        WIP HERE   ****

        (update this :v assoc (- n fcnt) x))))




  (containsKey [this i] (and (int? i) (< i (count this))))
  (entryAt [this i]
    (when (and (int? i) (< i (count this)))
      [i (nth this i)]))

  clojure.lang.IPersistentStack
  (peek [this] (if (pos? (count v))
                 (peek v)
                 (nth rfront (dec (count rfront)) nil)))
  (pop [this] (if (pos? (count (:v this)))
                (update this :v pop)
                (update this :rfront subvec 1)))

  clojure.lang.Reversible
    (rseq [this] (concat (rseq v) rfront))

    ;;; automatic for defrecord -- can't override and shouldn't -- need deftype
  clojure.lang.ILookup
     (valAt [this i] (nth this i))
     (valAt [this i default] (nth this i default))
  )



