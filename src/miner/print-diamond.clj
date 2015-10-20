;; https://gist.github.com/trikitrok/378a8af7d9a4295a93f5

;; http://garajeando.blogspot.com/2015/08/kata-print-diamond-in-clojure.html

;; Given a letter, print a diamond starting with ‘A’ with the supplied letter at the widest
;; point.
;; SEM: better to use dots and pluses instead of spaces



;; (print-diamond \C) will print
;; ..A..
;; .B+B.
;; C+++C
;; .B+B.
;; ..A..



;; Key points:
;; 1. Separate calculation of data from presentation.
;; 2. Separate I/O (printing) from pure functions (calculations)
;; 3. Avoid doing a lot of string manipulations.  It's better to use Clojure collections.
;; 4. Convert to longs to do arithmetic, then convert back once for presentation.

;; Note: it's almost always better to be zero-based in Clojure.  I wrote a version which was
;; one-based (A=1 seemed natural), but it was fiddly.  Zero-based is more natural for
;; Clojure code.

;; zero-based A=0 .. Z=25
(defn nch [n]
  (char (+ n (long \A))))

(defn chnum [ch]
  (- (long ch) (long \A)))

;; zero-based Cs -- long offsets from (long \A)
;; cmax is max char (inclusive), cnum is this line's char
;; returns [\C n-before m-between]
(defn diamond-line-data [cmax cnum]
  {:pre [(<= 0 cnum cmax)]}
  (let [pre (- cmax cnum)
        btw (dec (* 2 cnum))]
    [(nch cnum) pre btw]))

(defn reflect-seq [xs]
  (concat xs (rest (reverse xs))))

;; calculate top part of diamond, then use reflection for bottom half
(defn diamond-lines [cmax]
  (reflect-seq (map (partial diamond-line-data cmax) (range (inc cmax)))))

(defn print-diamond [ch]
  (doseq [[c pre btw] (diamond-lines (chnum ch))]
    (dotimes [i pre] (print "."))
    (print c)
    (when (pos? btw)
      (dotimes [i btw] (print "+"))
      (print c))
    (dotimes [i pre] (print "."))
    (println)))


#_
(defn vreflection [xs]
  (let [v (vec xs)]
    (into v (rest (rseq v)))))

