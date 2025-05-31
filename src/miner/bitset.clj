(ns miner.bitset)

;; SEM experiment.  Using a long as a "set" of bits containing 0-63.  See also Long/SIZE
;; (64) and Long/MAX_VALUE for limits.
;; For serious work, look at java.util.BitSet but beware that it's mutable and not thread-safe.

;; set-like functions but with names beginning with b.
;;
;;  Think of a long integer as a sorted-set of 0-63.  If your problem maps into a small set,
;;  it can be much faster than using a generic Clojure set.

;;  "bpeek" returns index of lowest set bit.  "bpop" disjoins the lowest bit.  "bseq" yields
;;  a seq in ascending order of bit indices.  "brseq" gives you reverse order.


(def ^:const b-all -1)

(def ^:const b-empty 0)

(def bempty? zero?)

(defn bcount [b]
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

(def bcomplement bit-not)

;; low to high seems the natural order
;; lazy impl was slow and didn't seem worth it

;; max bit index
(defn bmax [n]
  (when-not (zero? n)
    (- (dec Long/SIZE) (Long/numberOfLeadingZeros n))))

;; min bit
(defn bpeek [n]
  (when-not (zero? n)
    (Long/numberOfTrailingZeros n)))

(defn bpop [n]
  (bit-xor n (Long/lowestOneBit n)))


;; returns vec of single-bit longs (powers of 2) from n, lowest first
(defn bsingles [n]
  (loop [n n bs (transient [])]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/lowestOneBit n)]
        (recur (bit-xor n h) (conj! bs h))))))

;; low to high
(defn bvec [n]
  (loop [n n bs (transient [])]
    (if (zero? n)
      (persistent! bs)
      (recur (bit-xor n (Long/lowestOneBit n)) (conj! bs (Long/numberOfTrailingZeros n))))))

;;; natural order is least bit first.  Use brseq if you want reverse order (biggest first)
(defn bseq [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/highestOneBit n)]
        (recur (bit-xor n h) (conj bs (Long/numberOfTrailingZeros h)))))))

(defn brseq [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (recur (bit-xor n (Long/lowestOneBit n)) (conj bs (Long/numberOfTrailingZeros n))))))

;; lazy but slow, better to be eager for small things
#_
(defn lbseq [n]
  (sequence (comp (take-while (complement zero?)) (map bpeek)) (iterate bpop n)))

#_
(defn xbvec [n]
  (into [] (comp (take-while (complement zero?)) (map bpeek)) (iterate bpop n)))

;; convert to conventional Clojure Set of longs (indices of marked bits)
(defn bset->set [n]
  (loop [n n bs (transient #{})]
    (if (zero? n)
      (persistent! bs)
      (recur (bit-xor n (Long/lowestOneBit n)) (conj! bs (Long/numberOfTrailingZeros n))))))

(defn bsingle? [n]
  (= (Long/bitCount n) 1))

(defn bsubset? [bsub bsuper]
  (= bsuper (bit-or bsub bsuper)))

(defn bsuperset? [bsuper bsub]
  (bsubset? bsub bsuper))

;; could be smarter and avoid the bseq
(defn breduce [f init n]
  (reduce f init (bseq n)))

;; no prefix, but you could add "2r" if you need it
(defn bstr
  ([n] (Long/toBinaryString n))
  ([width n]
   {:pre [(< -1 width Long/SIZE)]}
   (let [bs (bstr n)
         pad (- width (count bs))]
     (if (pos? pad)
       (str (subs "0000000000000000000000000000000000000000000000000000000000000000" 0 pad)
            bs)
       bs))))


;; I like the 0x prefix. `width` refers to hex digits, not including 0x prefix
(defn hexstr
  ([n] (str "0x" (clojure.string/upper-case (Long/toHexString n))))
  ([width n]
   {:pre [(<= 0 width 16)]}
   (let [hs (hexstr n)
         len (- (count hs) 2)]
     (if (> width len)
       (str "0x" (subs "0000000000000000" 0 (- width len)) (subs hs 2))
       hs))))
