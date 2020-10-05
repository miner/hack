(ns miner.bitset)

;; SEM experiment.  Using a long as a "set" of bits containing 0-63.
;; For serious work, look at java.util.BitSet but beware that it's mutable and not thread-safe.

;; set-like functions but with names beginning with b

(def ^:const b-all -1)

(def ^:const b-empty 0)


(defn bcount [^long b]
  (Long/bitCount b))

(def bempty? zero?)
  
(defn bconj
  ([b i] (bit-set b i))
  ([b i & more] (reduce bit-set (bit-set b i) more)))

(defn bdisj
  ([b i]  (bit-clear b i))
  ([b i & more] (reduce bit-clear (bit-clear b i) more)))

(defn bcontains?
  ([b i] (bit-test b i))
  ([b i & more] (and (bit-test b i)
                     (empty? (sequence (remove #(bit-test b %)) more)))))

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

;; returns seq of single bit longs (powers of 2) from n
(defn bsingles [^long n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj bs h))))))

;; returns vector of marked bit indices  (slightly faster if you don't need a real Clojure Set)
(defn bindices [^long n]
  (loop [n n bs (transient [])]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/highestOneBit n)]
        (recur (bit-and-not n h) (conj! bs (Long/numberOfTrailingZeros h)))))))

;; convert to conventional Clojure Set of longs (indices of marked bits)
(defn bset [^long n]
  (loop [n n bs (transient #{})]
    (if (zero? n)
      (persistent! bs)
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj! bs (Long/numberOfTrailingZeros h)))))))

;; returns index of high bit 63-0 or nil if n=0
(defn high-bit-index [^long n]
  (when-not (zero? n)
    (Long/numberOfTrailingZeros (Long/highestOneBit n))))

(defn low-bit-index [^long n]
  (when-not (zero? n)
    (Long/numberOfTrailingZeros (Long/lowestOneBit n))))

(defn bsingle? [^long n]
  (= (Long/bitCount n) 1))
  

(defn bsubset? [bsub bsuper]
  (= bsuper (bit-or bsub bsuper)))

(defn bsuperset? [bsuper bsub]
  (bsubset? bsub bsuper))

(defn bstr
  ([^long n] (Long/toBinaryString n))
  ([^long n width]
   {:pre [(<= 0 width 64)]}
   (let [bs (bstr n)
         pad (- width (count bs))]
     (if (pos? pad)
       (str (subs "0000000000000000000000000000000000000000000000000000000000000000" 0 pad)
            bs)
       bs))))

(defn hexstr
  ([^long n] (clojure.string/upper-case (Long/toHexString n)))
  ([^long n width]
   {:pre [(<= 0 width 16)]}
   (let [hs (hexstr n)
         pad (- width (count hs))]
     (if (pos? pad)
       (str (subs "0000000000000000" 0 pad) hs)
       hs))))



(defn hexstr2
  ([^long n] (clojure.string/upper-case (Long/toHexString n)))
  ([^long n width]
   {:pre [(<= 0 width 16)]}
   (let [hs (hexstr n)
         len (count hs)]
     (if (> width len)
       (str (subs "0000000000000000" len) hs)
       hs))))
