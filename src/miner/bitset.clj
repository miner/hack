(ns miner.bitset)

;; SEM experiment.  Using a long as a "set" of bits containing 0-63.
;; For serious work, look at java.util.BitSet but beware that it's mutable and not thread-safe.

;; set-like functions but with names beginning with b.
;;
;;  Think of a long integer as a sorted-set of 0-63.  If your problem maps into a small set,
;;  it can be much faster than using a generic Clojure set.

;;  You can depend on the "bfirst" being the smallest bit index, and "blast" being the
;;  greatest.  "brest" disjoins the bfirst element (smallest).  "bseq" yields a seq in
;;  ascending order.  "brseq" gives you reverse order.


(def ^:const b-all -1)

(def ^:const b-empty 0)

(def bempty? zero?)

(defn bcount [^long b]
  (Long/bitCount b))

(defn bconj
  ([b i] (bit-set b i))
  ([b i & more] (reduce bit-set (bit-set b i) more)))

(defn bdisj
  ([b i]  (bit-clear b i))
  ([b i & more] (reduce bit-clear (bit-clear b i) more)))

(defn bcontains?
  ([b i] (bit-test b i))
  ([b i & more] (and (bit-test b i)
                     (every? #(bit-test b %) more))))

;; indices is collection of longs
(defn bsome? [b indices]
  (some #(bit-test b %) indices))

;; bany? takes 1-or-more args for the indices.
;; (bany? b a1 a2 ... aN) = (bsome? b (a1 a2 ... aN))
;; not sure if we need both
;; might be better to bit-set a bunch and then do one bit-and?
(defn bany?
  ([b i] (bit-test b i))
  ([b i j] (or (bit-test b i) (bit-test b j)))
  ([b i j & more] (or (bit-test b i) (bit-test b j) (some #(bit-test b %) more))))

(def bunion bit-or)

(def bdifference bit-and-not)

(def bintersection bit-and)

(defn bcomplement [n]
  (bit-xor n b-all))

;; FIXME: Do you care about order?  low to high seems more natural
;; FIXME: What about lazy?  Probably don't care as they're small


;; min bit
(defn bfirst [n]
  (when-not (zero? n)
    (Long/numberOfTrailingZeros n)))

;; max bit
(defn blast [n]
  (when-not (zero? n)
    (- 63 (Long/numberOfLeadingZeros n))))

(defn brest [n]
  (when-not (zero? n)
    (bit-and-not n (Long/lowestOneBit n))))


;; returns seq of single-bit longs (powers of 2) from n, lowest first
(defn bsingles [n]
  (loop [n n bs (transient [])]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj! bs h))))))

;; low to high
(defn bvec [n]
  (loop [n n bs (transient [])]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj! bs (Long/numberOfTrailingZeros n)))))))

;;; natural order is least bit first.  Use brseq if you want reverse order (biggest first)
(defn bseq [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/highestOneBit n)]
        (recur (bit-and-not n h) (conj bs (Long/numberOfTrailingZeros h)))))))

;; lazy but slow, better to be eager for small things
#_
(defn lbseq [n]
  (sequence (comp (take-while (complement zero?)) (map bfirst)) (iterate brest n)))


(defn brseq [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (recur (bit-and-not n (Long/lowestOneBit n)) (conj bs (Long/numberOfTrailingZeros n))))))

;; convert to conventional Clojure Set of longs (indices of marked bits)
(defn bset [n]
  (loop [n n bs (transient #{})]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj! bs (Long/numberOfTrailingZeros h)))))))

(defn bsingle? [n]
  (= (Long/bitCount n) 1))

(defn bsubset? [bsub bsuper]
  (= bsuper (bit-or bsub bsuper)))

(defn bsuperset? [bsuper bsub]
  (bsubset? bsub bsuper))


(defn breduce [f init n]
  (reduce f init (bseq n)))

(defn bstr
  ([n] (Long/toBinaryString n))
  ([width n]
   {:pre [(<= 0 width 64)]}
   (let [bs (bstr n)
         pad (- width (count bs))]
     (if (pos? pad)
       (str (subs "0000000000000000000000000000000000000000000000000000000000000000" 0 pad)
            bs)
       bs))))

(defn hexstr
  ([n] (clojure.string/upper-case (Long/toHexString n)))
  ([width n]
   {:pre [(<= 0 width 16)]}
   (let [hs (hexstr n)
         pad (- width (count hs))]
     (if (pos? pad)
       (str (subs "0000000000000000" 0 pad) hs)
       hs))))

(defn hexstr2
  ([n] (clojure.string/upper-case (Long/toHexString n)))
  ([width n]
   {:pre [(<= 0 width 16)]}
   (let [hs (hexstr n)
         len (count hs)]
     (if (> width len)
       (str (subs "0000000000000000" len) hs)
       hs))))



;;; Retired code

;; returns seq of single-bit longs (powers of 2) from n, highest first
#_
(defn bsingles1 [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj bs h))))))

;; returns vector of marked bit indices  (slightly faster if you don't need a real Clojure
;; Set)
;;  Actually, I think bseq is more natural.  Lowest to highest (similar to range, dotimes, etc)
#_
(defn bindices-hilo [^long n]
  (loop [n n bs (transient [])]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/highestOneBit n)]
        (recur (bit-and-not n h) (conj! bs (Long/numberOfTrailingZeros h)))))))

