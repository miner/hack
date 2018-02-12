;; NOT REALLY for this project.


;; See bug (and reason for declining) for how to deal with iterator issues in Clojure
;; https://dev.clojure.org/jira/browse/CLJ-1738
(defn iter-seq [iter f]
  (if (.hasNext iter)
    (lazy-seq
      (cons (f (.next iter))
            (iter-seq iter f)))))

(defn mutating-iterable
  ([stop] (mutating-iterable 0 stop))
  ([start stop]
   (reify Iterable
     (iterator [_]
       (let [value (java.util.concurrent.atomic.AtomicLong. (dec start))]
         (reify java.util.Iterator
           (next [this]
             (if (.hasNext this)
               (doto value (.incrementAndGet))
               (throw (java.util.NoSuchElementException.))))
           (hasNext [_]
             (< (.get value) (dec stop)))
           (remove [_])))))))

;; Issue is that iterator reuses mutatable cell
;; (iterator-seq (.iterator (mutating-iterable 5)))
;; => (4 4 4 4 4)
;;
;; (iter-seq (.iterator (mutating-iterable 5)) #(.get %))
;; => (0 1 2 3 4)
;;
;; Surprisingly,
;; (iter-seq (.iterator (mutating-iterable 5)) identity)
;; => (1 2 3 4 4)




;; SEM: that doto isn't very friendly for our application.  We want the long result, not the
;; wrapper object AtomicLong to be returned.  Somewhat simpler logic if we handle the result
;; better.   Ah, but the original point was to return something mutable so that the results
;; would get screwed up by laziness. In which case, iter-seq, tries to manage the issue
;; with the lazy-seq approach and a function that transforms to an immutable value.  Fine,
;; but seems unintentional.  Also, the original is using an Iterable -- so you have to ask
;; for .iterator to make things work.  I cut that out to simplify.

(defn mutiter
  ([stop] (mutiter 0 stop))
  ([start stop]
   (let [value (java.util.concurrent.atomic.AtomicLong. start)]
     (reify java.util.Iterator
       (next [this]
         (if (.hasNext this)
           (.getAndIncrement value)
           (throw (java.util.NoSuchElementException.))))
       (hasNext [_]
         (< (.get value) stop))
       (remove [_])))))
