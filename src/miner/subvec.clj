(ns miner.subvec)

;; BUG: reduce-kv not supported on a subvec
;; https://dev.clojure.org/jira/browse/CLJ-2065

;; best approach might be like rangedIterator, but for kvreduceRange
;; http://dev.clojure.org/jira/browse/CLJ-1082

;; NOTE: (vec subv) returns subv as of 1.7
;; different than (into [] subv)

(def sv500 (subvec (vec (range 501)) 1))
(def v500 (into [] sv500))

;; Attempt to implement protocol
;; notice the args are different orders between kv-reduce and reduce-kv

;; works but should be faster with a native Java implementation in APersistentVector.java
;; only use if necessary
#_ (when-not (satisfies?   clojure.core.protocols/IKVReduce (subvec [1] 0))
  (extend-type clojure.lang.APersistentVector$SubVector
    clojure.core.protocols/IKVReduce
    (kv-reduce [subv f init]
      (transduce (map-indexed vector)
                 (fn ([ret] ret) ([ret [k v]] (f ret k v)))
                 init
                 subv))))

;; BY THE WAY, this transduce would work with any seq so maybe it should be the generic
;; Sequential implementation.  Ahhh, but be careful you didn't interfere with the fast path
;; for other types.  Let's not do that unless it's really useful.


;; interop should make it faster
(when-not (satisfies?   clojure.core.protocols/IKVReduce (subvec [1] 0))
  (extend-type clojure.lang.APersistentVector$SubVector
    clojure.core.protocols/IKVReduce
    (kv-reduce
      [subv f init]
      (let [cnt (.count subv)]
        (loop [k 0 ret init]
          (if (< k cnt)
            (let [val (.nth subv k)
                  ret (f ret k val)]
              (if (reduced? ret)
                @ret
                (recur (inc k) ret)))
            ret))))))


;; not so bad
(defn iter-kv-reduce
  [subv f init]
  (let [^java.util.Iterator iter (.iterator ^clojure.lang.APersistentVector$SubVector subv)]
    (loop [k 0 ret init]
      (if (.hasNext iter)
        (let [val (.next iter)
              ret (f ret k val)]
          (if (reduced? ret)
            @ret
            (recur (inc k) ret)))
        ret))))

;; type-hint was critical for performance, now just as good as iterator

(defn ikv-reduce  [^clojure.lang.APersistentVector$SubVector subv f init]
  (let [cnt (.count subv)]
    (loop [k 0 ret init]
      (if (< k cnt)
        (let [val (.nth subv k)
              ret (f ret k val)]
          (if (reduced? ret)
            @ret
            (recur (inc k) ret)))
        ret))))



(defn svrkv [f init ^clojure.lang.APersistentVector$SubVector subv]
  (reduce f init (map-indexed vector (iterator-seq (.iterator subv)))))



;; converting to vector is generally faster than faking it with (map-indexed vector) even
;; with transduce

;; BUT what about memory usage?  GC?

;; seems faster to just bite the bullet and copy into a vector.
;; Note: (vec subv) will not make a real PersistentVector, it just leaves the original subv.
;; (into [] subv) is necessary.  (The change to `vec` was introduced in Clojure 1.7.)

(defn redkv [f init subv]
  (.kvreduce ^clojure.lang.PersistentVector (into [] subv) f init))

;; better
(defn tredkv1 [f init subv]
  (transduce (map-indexed vector)
             (fn ([ret] ret) ([ret [k v]] (f ret k v)))
             init
             subv))



(defn tredkv2 [f init subv]
  (transduce (map-indexed list)
             (fn ([ret] ret) ([ret kv] (apply f ret kv)))
             init
             subv))

;; ugly and slow
(defn iredkv [f init subv]
  (reduce (fn [r [k v]] (f r k v)) init (into [] (map-indexed vector) subv)))

(defn slow-redkv [f init subv]
  (reduce (fn [ret [k v]] (f ret k v))
          init
          (map-indexed vector subv)))



(defn slowkv [res k v]
  (+ res (* k (inc v)) (quot (dec k) 3)))



;; ~$ clj18
;; Clojure 1.8.0
;; user=> (def sv (subvec [:a :b :c] 1 3))
;; #'user/sv
;; user=> sv
;; [:b :c]
;; user=> (type sv)
;; clojure.lang.APersistentVector$SubVector
;; user=> (reduce-kv conj [] sv)
;; IllegalArgumentException No implementation of method: :kv-reduce of protocol: #'clojure.core.protocols/IKVReduce found for class: clojure.lang.APersistentVector$SubVector  clojure.core/-cache-protocol-fn (core_deftype.clj:568)
;; user=> (type (vec sv))
;; clojure.lang.APersistentVector$SubVector
;; user=> ^D
;; ~$ clj17
;; Clojure 1.7.0
;; user=> (def sv (subvec [:a :b :c] 1 3))
;; #'user/sv
;; user=> (reduce-kv conj [] sv)
;; IllegalArgumentException No implementation of method: :kv-reduce of protocol: #'clojure.core.protocols/IKVReduce found for class: clojure.lang.APersistentVector$SubVector  clojure.core/-cache-protocol-fn (core_deftype.clj:554)
;; user=> (type (vec sv))
;; clojure.lang.APersistentVector$SubVector
;; user=> ^D
;; 
;; 
;; ~$ clj16
;; Clojure 1.6.0
;; user=> (def sv (subvec [:a :b :c] 1 3))
;; #'user/sv
;; 
;; user=> (type sv)
;; clojure.lang.APersistentVector$SubVector
;; 
;; user=>  (reduce-kv conj [] sv)
;; IllegalArgumentException No implementation of method: :kv-reduce of protocol: #'clojure.core.protocols/IKVReduce found for class: clojure.lang.APersistentVector$SubVector  clojure.core/-cache-protocol-fn (core_deftype.clj:544)
;; 
;; user=> (type (vec sv))
;; clojure.lang.PersistentVector
;; 
;; user=> (source vec)
;; (defn vec
;;   "Creates a new vector containing the contents of coll. Java arrays
;;   will be aliased and should not be modified."
;;   {:added "1.0"
;;    :static true}
;;   ([coll]
;;    (if (instance? java.util.Collection coll)
;;      (clojure.lang.LazilyPersistentVector/create coll)
;;      (. clojure.lang.LazilyPersistentVector (createOwning (to-array coll))))))
;; nil
;; 
