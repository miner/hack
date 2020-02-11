
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

