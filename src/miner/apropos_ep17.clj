;; No ns because not useful code

;; Apropos clojure #17

;; I prefer to stay as data.  Avoid too much string building just to do I/O.  Make a
;; "rendering" function to do the printing, I/O, etc.  The point is that the Clojure data is
;; flexible so do the manipulations in Clojure.  Once you lock into string formats, it's
;; more work to change them.  Also, it's easier to have multiple ways to render the same
;; data, rather than having to do a lot of conversions to make a format of string for each
;; application.

;; In my print-frame, I map `name` over the data -- that stringifies things.  You could use
;; `str` instead.  Or maybe it should be a parameter.

(defn print-pad
  ([x] (print x))
  ([pad x] (let [s (str x)
                 len (count s)]
             (print s)
             (dotimes [_ (- pad len)] (print " ")))))

(defn print-star [len]
  (dotimes [_ len] (print "*"))
  (println))


(defn print-header
  ([width] (print-header "**" width "**"))
  ([begstr width endstr]
   (print begstr)
   (dotimes [_ width] (print "*"))
   (println endstr)))

(defn print-frame [xs]
  (let [xs (map name xs)
        width (reduce max 0 (map count xs))]
    (print-header "/*" width "*\\")
    (doseq [x xs]
      (print "* ")
      (print-pad width x)
      (println " *"))
    (print-header "\\*" width "*/")))


