(ns miner.bitset)

;; SEM experiment.  Using a long as a "set" of bits containing 0-63.
;; For serious work, look at java.util.BitSet but beware that it's mutable and not thread-safe.

(defn bcount [^long b]
  (Long/bitCount b))

(defn bconj
  ([b i] (bit-set b i))
  ([b i & more] (reduce bit-set (bit-set b i) more)))

(defn bdisj
  ([b i]  (bit-clear b i))
  ([b i & more] (reduce bit-clear (bit-clear b i) more)))

(def bunion bit-or)

(def bdifference bit-and-not)

(def bintersection bit-and)

;; returns seq of single bit longs (powers of 2) from n
(defn bsingles ^longs [^long n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/lowestOneBit n)]
        (recur (bit-and-not n h) (conj bs h))))))

;; convert to conventional set of longs
(defn bset [n]
  (into  #{} (map #(Long/numberOfTrailingZeros %)) (bsingles n)))


(defn bsingle? [^long n]
  (= (Long/bitCount n) 1))
  

(defn bsubset? [bsub bsuper]
  (= bsuper (bit-or bsub bsuper)))

(defn bsuperset? [bsuper bsub]
  (bsubset? bsub bsuper))

(defn bstr [^long n]
  (Long/toBinaryString n))
