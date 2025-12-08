(ns miner.fractional-index)

;;; Fractional Index - a scheme for indexing that is good at finding in-between values so
;;; you never have to re-index a collection.

;;; just for learning

;;; https://observablehq.com/@dgreensp/implementing-fractional-indexing

;;; see also an existing Clojure port:
;;; https://github.com/logseq/clj-fractional-indexing


;;; motivated by using strings, note \~ is greater than \z if you need a char endpoint
;;; nil or \space sorts first

;;; I'm only supporting the base-62 version

(def base-62-string
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

(def base-62-map
  (merge (zipmap base-62-string (range))
         (zipmap (range) base-62-string)))

(def char-inc
  (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy"
          "BCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(def char-dec
  (zipmap "BCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
          "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxy"))

(defn first-char [str]
  (nth str 0))

(defn last-char [str]
  (nth str (dec (count str))))


;;; largest all nine long (18 digits) -- limits parsing
(def nine18 999999999999999999)

;;; probably faster to precompute all the inc-chars and dec-chars into separate maps
(defn xxxchar-inc [c]
  (when-not (= c \z)
    (get base-62-map (inc (get base-62-map c)))))

(defn xxxchar-dec [c]
  (when-not (= c \A)
    (get base-62-map (dec (get base-62-map c)))))


;;; findex "integer" part or "istr" encoding has "a0" in the center, "Z9" is lower.
;;; ... X000 ... X999, Y00 ... Y99, Z0 ... Z9, a0 ... a9, b00 ... b99, c000 ... c999 ...
(defn ilen [ch]
  (case ch
    (\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z)
    (+ (- (long \Z) (long ch)) 2)
    (\a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z)
    (+ (- (long ch) (long \a)) 2)
    nil))

;;; should be cached in a map
(defn head-niner [head]
  (read-string (apply str (repeat (dec (ilen head)) "9"))))


(defn ilength [findex]
  (if-let [len (ilen (first-char findex))]
    len
    (throw (ex-info (str "invalid findex: " findex) {:invalid findex}))))


(defn istr [findex]
  (subs findex (ilength findex)))


;;; for making findex, need to pad internal 0s

(defn padzero [n]
  (when (pos? n)
    (subs "00000000000000000000000000" 0 n)))



;;; Note: cannot use parse-long as num might be bigger than long.  read-string will convert
;;; to bigint if necessary.
;;; read-string "08" tries to do octacl and gets error, so we need to skip leading 0s

(defn read-num [s]
  (let [len (count s)
        start (reduce (fn [r i] (if (= (nth s i) \0) r (reduced i))) 0 (range len))]
    (if (zero? start)
      (read-string s)
      (read-string (subs s start)))))


;;; BUG -- need to return nil if you hit max/min istr


(defn inc-istr [istr]
  (let [head (first-char istr)
        num1 (inc (read-num (subs istr 1)))]
    (if (> num1 (head-niner head))
      (when-let [h1 (char-inc head)]
        (apply str h1 (repeat (dec (ilen h1)) "0")))
      (let [nstr (str num1)
            width (count nstr)
            pad (- (dec (ilen head)) width)]
        (str head (padzero pad) nstr)))))


  
(defn dec-istr [istr]
  (let [head (first-char istr)
        num (dec (read-num (subs istr 1)))]
    (if (neg? num)
      (when-let [h1 (char-dec head)]
        (apply str h1 (repeat (dec (ilen h1)) "9")))
      (let [nstr (str num)
            width (count nstr)
            pad (- (dec (ilen head)) width)]
          (str head (padzero pad) nstr)))))


(defn imid [i j]
  (quot (+ (inc i) j) 2))

(def smallest-istr (str \A (padzero (dec (ilength "A")))))

(defn midfrac [fa fb]
  

  )


(defn midpoint [a b]
  (if (and (nil? a) (nil? b))
    "a0"
    (if (nil? a)
      (let [bistr (istr b)]
        (if (= bistr smallest-istr)
          (str bistr (midfrac nil (subs b (count bistr))))

    (let [n (when b
              (first (keep-indexed (fn [i _c] (when-not (= (nth a i \0) (nth b i)) i)) b)))]
      (if (and n (pos? n))
        (str (subs b 0 n) (midpoint (subs a n) (subs b n)))
        (let [digit-a (if (seq a) (base-62-map (first-char a)) 0)
              digit-b (if b (base-62-map (first b)) 62)]
          ;; (prn :debug
          ;;      :a a :b b
          ;;      :digit-a digit-a :digit-b digit-b)
          (if (> (- digit-b digit-a) 1)
            (str (base-62-map (imid digit-a digit-b)))
            (if (and (seq b) (> (count b) 1))
              (subs b 0 1)
              (str (base-62-map digit-a) (midpoint (subs a 1) nil))))))))




;;; save for ref  --- buggy
(defn orig-midpoint [a b digits]
  (let [zero \0
        str-slice subs]
    (when (and b (>= (compare a b) 0))
      (throw (ex-info (str a " >= " b) {:a a :b b})))
    #_ (when (or (= (last-char a) zero) (= (last-char b) zero))
      (throw (ex-info " trailing zero" {:a a :b b})))
    (let [n (when b
              (first (keep-indexed (fn [i _c] (when-not (= (nth a i zero) (nth b i)) i)) b)))]
      (if (and n (> n 0))
        (str (str-slice b 0 n) (orig-midpoint (str-slice a n) (str-slice b n) digits))
        (let [digit-a (if (seq a) (.indexOf digits (str (first a))) 0)
              digit-b (if b (.indexOf digits (str (first b))) (count digits))]
          ;; (prn :debug
          ;;      :a a :b b
          ;;      :digit-a digit-a :digit-b digit-b)
          (if (> (- digit-b digit-a) 1)
            (str (nth digits (int (Math/round (* 0.5 (+ digit-a digit-b))))))
            (if (and (seq b) (> (count b) 1))
              (str-slice b 0 1)
              (str (nth digits digit-a) (orig-midpoint (str-slice a 1) nil digits)))))))))

(defn omp [a b]
  (orig-midpoint a b base-62-string))


(comment



(defn generate-key-between
  [a b & {:keys [digits]
          :or {digits base-62-digits}}]
  ;; (prn :debug :generate :a a :b b)
  (when a (validate-order-key a digits))
  (when b (validate-order-key b digits))
  (when (and a b (>= (compare a b) 0))
    (throw (ex-info (str a " >= " b) {:a a :b b})))
  (let [result (cond
                 (nil? a) (if (nil? b)
                            (str "a" (first digits))
                            (let [ib (get-integer-part b)
                                  fb (str-slice b (count ib))]
                              (if (= ib (str "A" (apply str (repeat 26 (first digits)))))
                                (str ib (midpoint "" fb digits))
                                (if (< (compare (str ib) b) 0)
                                  (str ib (midpoint "" fb digits))
                                  (let [res (decrement-integer ib digits)]
                                    (if (nil? res)
                                      (throw (ex-info "cannot decrement any more" {:a a :b b :ib ib}))
                                      res))))))
                 (nil? b) (let [ia (get-integer-part a)
                                fa (str-slice a (count ia))
                                i (increment-integer ia digits)]
                            (if (nil? i)
                              (str ia (midpoint fa nil digits))
                              i))
                 :else (let [ia (get-integer-part a)
                             fa (str-slice a (count ia))
                             ib (get-integer-part b)
                             fb (str-slice b (count ib))]
                         ;; (prn :debug :ia ia :ib ib :fa fa :fb fb :b b)
                         (if (= ia ib)
                           (str ia (midpoint fa fb digits))
                           (let [i (increment-integer ia digits)]
                             ;; (prn :debug :i i :fa fa)
                             (if (nil? i)
                               (throw (ex-info "cannot increment any more" {:a a
                                                                            :b b
                                                                            :ia ia}))
                               (if (< (compare i b) 0) i (str ia (midpoint fa nil digits))))))))]
    (if (or (and a (>= (compare a result) 0))
            (and b (>= (compare result b) 0)))
      (throw (ex-info "generate-key-between failed"
                      {:a a
                       :b b
                       :between result}))
      result)))

(defn generate-n-keys-between
  [a b n & {:keys [digits]
            :or {digits base-62-digits}}]
  ;; (prn :debug :generate-n-keys-between :a a :b b :n n)
  (let [result (cond
                 (= n 0) []
                 (= n 1) [(generate-key-between a b {:digits digits})]
                 (nil? b) (reduce
                           (fn [col _]
                             (let [k (generate-key-between (or (peek col) a) b {:digits digits})]
                               (conj col k)))
                           []
                           (range n))
                 (nil? a) (->>
                           (reduce
                            (fn [col _]
                              (let [k (generate-key-between a (or (peek col) b) {:digits digits})]
                                (conj col k)))
                            []
                            (range n))
                           (reverse)
                           (vec))
                 :else (let [mid (int (Math/floor (/ n 2)))
                             c (generate-key-between a b {:digits digits})]
                         (concat
                          (generate-n-keys-between a c mid {:digits digits})
                          [c]
                          (generate-n-keys-between c b (- n mid 1) {:digits digits}))))]
    (vec (take n result))))

;; end comment
)

