(ns miner.morton)

;;; This is just an experiment.  Don't use this for real work.  Get a proper library
;;; instead.

;;; https://en.wikipedia.org/wiki/Z-order_curve

;;; A Morton Code is the bitwise interleaving of multiple coordinates into a single integer.
;;; This supports some fast ways to sort points, etc. and implement quad-trees.

;;; Experiment with naive approach for validation.  A faster, better approach uses
;;; precomputed tables.

;;; First you need to quantize the units.  We will use a long (64 bits) for the Morton Code.
;;; Half the bits are X, and half are Y, so they can each be int (32 bits).

(defn interleave-bits [x y]
  (loop [z 0 i 0]
    (if (= i 32)
      z
      (recur (long (cond-> z
               (bit-test x i) (bit-set (* 2 i))
               (bit-test y i) (bit-set (inc (* 2 i)))))
             (inc i)))))

;;; slightly faster
(defn zbits1 [x y]
  (loop [z 0 i 32]
    (if (neg? i)
      z
      (recur (long (cond-> z
               (bit-test x i) (bit-set (* 2 i))
               (bit-test y i) (bit-set (inc (* 2 i)))))
             (dec i)))))


;;; borrowed from my bitset.clj
#_
(defn bseq [n]
  (loop [n n bs ()]
    (if (zero? n)
      bs
      (let [h (Long/highestOneBit n)]
        (recur (bit-xor n h) (conj bs (Long/numberOfTrailingZeros h)))))))


;;; much faster to hit only set bits
(defn zbits2 [x y]
  (let [z (loop [z 0 n x]
            (if (zero? n)
              z
              (let [h (Long/highestOneBit n)]
                (recur (long (bit-set z (* 2 (Long/numberOfTrailingZeros h)))) (bit-xor n h)))))]
    (loop [z z n y]
      (if (zero? n)
        z
        (let [h (Long/highestOneBit n)]
          (recur (bit-set z (inc (* 2 (Long/numberOfTrailingZeros h)))) (bit-xor n h)))))))

  

;;; fastest so far (was zbits4)
(defn zbits [x y]
  (loop [z 0 n x iplus 0]
    (if (zero? n)
      (if (zero? iplus) (recur z y 1) z)
      (let [h (Long/highestOneBit n)]
        (recur (long (bit-set z (+ iplus (* 2 (Long/numberOfTrailingZeros h)))))
               (bit-xor n h)
               iplus)))))




;;; Faster ideas:
;;; https://graphics.stanford.edu/~seander/bithacks.html#InterleaveTableObvious

;;; built a lookup table of 256 entries
(defn lut256 []
  (sort-by peek (for [x (range 16) y (range 16)]
                  (vector x y (bit-or (bit-shift-left y 4) x)
                          (zbits4 x y)))))


;;; new idea, add pre-shifted balue as well
;;; Not sure about offsets, needs thinking and testing
#_
(defn shifted-pairs [[yx ib]]
  (map #(vector (bit-shift-left yx (* % 4)) (bit-shift-left ib (* 8 %))) (range 8)))


;;; try basic map for [4bits y, 4 bits x] as int, mapped into interleaved bits
(def myxib (into {}
                 (for [x (range 16) y (range 16)]
                   (vector (bit-or (bit-shift-left y 4) x)
                           (zbits4 x y)))))

;;; THis should always be true as the interleaved bits should have the same count
#_
(assert (every? (fn [[a b]] (= (Long/bitCount a) (Long/bitCount b))) myxib))


;;; unsigned- shouldn't matter as x and y should be 32 bit unsigned ints, but not checked
(defn lutbits [x y]
  (let [y (bit-shift-left y 4)]
    (loop [nib 7 z 0]
      (if (neg? nib)
        z
        (let [yx (bit-or (bit-and 0xF0 (unsigned-bit-shift-right y (* 4 nib)))
                         (bit-and 0xF (unsigned-bit-shift-right x (* 4 nib))))]
          (recur (dec nib)
                 (bit-or z (bit-shift-left (myxib yx) (* 8 nib)))))))))


(def vyxib (into [] (map val) (sort (seq myxib))))

;;; much faster to index into vector, rather than int key of map
;;; but still slower than zbits
(defn vlutbits [x y]
  (let [y (bit-shift-left y 4)]
    (loop [nib 7 z 0]
      (if (neg? nib)
        z
        (let [yx (bit-or (bit-and 0xF0 (unsigned-bit-shift-right y (* 4 nib)))
                         (bit-and 0xF (unsigned-bit-shift-right x (* 4 nib))))]
          (recur (dec nib)
                 (bit-or z (bit-shift-left (vyxib yx) (* 8 nib)))))))))



;;; should consider byte-array ???  However, there's an issue with the bytes always being
;;; signed so you need to check sign and convert to appropriate int/long, which kind of
;;; complicates the code.  Not implemented.
