
;;; stolen from clojure.string...

(defn ^String cs-escape
  "Return a new string, using cmap to escape each character ch
   from s as follows:
   
   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."
  {:added "1.2"}
  [^CharSequence s cmap]
  (loop [index (int 0)
         buffer (StringBuilder. (.length s))]
    (if (= (.length s) index)
      (.toString buffer)
      (let [ch (.charAt s index)]
        (if-let [replacement (cmap ch)]
          (.append buffer replacement)
          (.append buffer ch))
        (recur (inc index) buffer)))))


;;; But my clever changes actually made it slower.
#_ (defn ^String my-escape
  "Return a new string, using cmap to escape each character ch
   from s as follows:
   
   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."

  [^CharSequence s cmap]
  (let [len (.length s)]
    (loop [index (int 0)
	  buffer nil]
      (if (= len index) ; terminate
	(if buffer (.toString ^StringBuilder buffer) s)
	(let [ch (.charAt s index)
	      replacement (cmap ch)
	      buffer (or buffer (and replacement (.append ^StringBuilder (StringBuilder. len) s 0 index)))]
	  (if buffer (.append ^StringBuilder buffer (or replacement ch)))
	  (recur (inc index) buffer))))))


;; convenience hack
(defn abc [n]
  {:pre [(<= 1 n 26)]}
  (str (char (+ (dec (long \a)) n))))




;; Per Clojure guidance: don't extend a protocol that you don't own to a type that you don't
;; own.  But what about Java types?  Well, you could be changing things for other people,
;; maybe dangerous?

#_
(extend-protocol clojure.core.protocols/IKVReduce
  java.lang.String
  (kv-reduce [^String s f init]
    (let [cnt (.length s)]
      (loop [i 0 res init]
        (if (< i cnt)
          (let [ret (f res i (.charAt s i))]
            (if (reduced? ret)
              @ret
              (recur (inc i) ret)))
          res)))))


;; Unfinished ideas about making String reversible.  Really want rseq to work, but would
;; need a new protocol and method that defaults to rseq for implementation.
#_
(defprotocol IReversible
  (rev-seq [coll]))

;; String Iterable 
;; should return an IReduceInit to be fast?


;; faster for later
;;[revsb (.reverse (StringBuilder phrase))]



