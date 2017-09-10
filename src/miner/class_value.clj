(ns miner.class-value)


;; Testing shows no particular benefit compared to built-in memoize.  Consider this a
;; learning experience.

;; ClassValue is not suitable for use in clojure contrib core.memo or core.cache.  API is
;; too different to fit in.  But it could use similar API with its own implementation.  The
;; bad news is that it's just not worth it.

;; should be thread-safe (at least for the memoized function)
;; but needs testing
;; memo-clear! is probably thread-safe but might be confusing if used a lot

(defn memo
  "Memoize a function f that takes a single Class argument."
  [f]
  {:pre [(fn? f)]}
  (let [cache (proxy [java.lang.ClassValue] [] (computeValue [^Class c] (f c)))]
    (with-meta 
      (fn [^Class c] (.get ^java.lang.ClassValue cache c))
      {::cache cache
       ::original f})))

(defn memoized? [f]
  (boolean (::cache (meta f))))

(defn memo-clear!
  "Reaches into a memoized function and clears the cache for given classes.  This is a
   destructive operation and should be used with care."
  [f & classes]
  (when-let [cache (::cache (meta f))]
    (doseq [c classes]
      (.remove ^java.lang.ClassValue cache ^Class c))))



;; need a stress test with lots of threads


(defn slow [^Class c] (Thread/sleep 30) (count (str c)))


(defn testing 
  ([] (testing slow))
  ([f] (let [classes (take 1000 (cycle [Integer String java.lang.ClassValue Long Float Double]))]
         (reduce + 0 (map f classes)))))

(defn agtest [state f]
  (+ state (testing f)))

(defn thread-testing [f]
  (let [agents (repeatedly 100 #(send (agent 0) agtest f))]
    (apply await agents)
    (reduce + 0 (map deref agents))))


