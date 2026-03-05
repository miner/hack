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


;;; slightly faster
(defn zbits1 [x y]
  (loop [z 0 i 32]
    (if (neg? i)
      z
      (recur (long (cond-> z
               (bit-test x i) (bit-set (* 2 i))
               (bit-test y i) (bit-set (inc (* 2 i)))))
             (dec i)))))

;;; much faster to hit only set bits
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



(defn spread-bits [n]
  ;; (assert (zero? (bit-and 0xFFFFFFFF n)))
  (loop [z 0 n n]
    (if (zero? n)
      z
      (let [h (Long/highestOneBit n)]
        (recur (long (bit-set z (* 2 (Long/numberOfTrailingZeros h)))) (bit-xor n h))))))


;;; probably better to use table of 8 bit spread bits and do lookups and shifts later
(def morton256  (mapv spread-bits (range 256)))

#_
(def precalc-morton256
  [ 0x0000, 0x0001, 0x0004, 0x0005, 0x0010, 0x0011, 0x0014, 0x0015, 
  0x0040, 0x0041, 0x0044, 0x0045, 0x0050, 0x0051, 0x0054, 0x0055, 
  0x0100, 0x0101, 0x0104, 0x0105, 0x0110, 0x0111, 0x0114, 0x0115, 
  0x0140, 0x0141, 0x0144, 0x0145, 0x0150, 0x0151, 0x0154, 0x0155, 
  0x0400, 0x0401, 0x0404, 0x0405, 0x0410, 0x0411, 0x0414, 0x0415, 
  0x0440, 0x0441, 0x0444, 0x0445, 0x0450, 0x0451, 0x0454, 0x0455, 
  0x0500, 0x0501, 0x0504, 0x0505, 0x0510, 0x0511, 0x0514, 0x0515, 
  0x0540, 0x0541, 0x0544, 0x0545, 0x0550, 0x0551, 0x0554, 0x0555, 
  0x1000, 0x1001, 0x1004, 0x1005, 0x1010, 0x1011, 0x1014, 0x1015, 
  0x1040, 0x1041, 0x1044, 0x1045, 0x1050, 0x1051, 0x1054, 0x1055, 
  0x1100, 0x1101, 0x1104, 0x1105, 0x1110, 0x1111, 0x1114, 0x1115, 
  0x1140, 0x1141, 0x1144, 0x1145, 0x1150, 0x1151, 0x1154, 0x1155, 
  0x1400, 0x1401, 0x1404, 0x1405, 0x1410, 0x1411, 0x1414, 0x1415, 
  0x1440, 0x1441, 0x1444, 0x1445, 0x1450, 0x1451, 0x1454, 0x1455, 
  0x1500, 0x1501, 0x1504, 0x1505, 0x1510, 0x1511, 0x1514, 0x1515, 
  0x1540, 0x1541, 0x1544, 0x1545, 0x1550, 0x1551, 0x1554, 0x1555, 
  0x4000, 0x4001, 0x4004, 0x4005, 0x4010, 0x4011, 0x4014, 0x4015, 
  0x4040, 0x4041, 0x4044, 0x4045, 0x4050, 0x4051, 0x4054, 0x4055, 
  0x4100, 0x4101, 0x4104, 0x4105, 0x4110, 0x4111, 0x4114, 0x4115, 
  0x4140, 0x4141, 0x4144, 0x4145, 0x4150, 0x4151, 0x4154, 0x4155, 
  0x4400, 0x4401, 0x4404, 0x4405, 0x4410, 0x4411, 0x4414, 0x4415, 
  0x4440, 0x4441, 0x4444, 0x4445, 0x4450, 0x4451, 0x4454, 0x4455, 
  0x4500, 0x4501, 0x4504, 0x4505, 0x4510, 0x4511, 0x4514, 0x4515, 
  0x4540, 0x4541, 0x4544, 0x4545, 0x4550, 0x4551, 0x4554, 0x4555, 
  0x5000, 0x5001, 0x5004, 0x5005, 0x5010, 0x5011, 0x5014, 0x5015, 
  0x5040, 0x5041, 0x5044, 0x5045, 0x5050, 0x5051, 0x5054, 0x5055, 
  0x5100, 0x5101, 0x5104, 0x5105, 0x5110, 0x5111, 0x5114, 0x5115, 
  0x5140, 0x5141, 0x5144, 0x5145, 0x5150, 0x5151, 0x5154, 0x5155, 
  0x5400, 0x5401, 0x5404, 0x5405, 0x5410, 0x5411, 0x5414, 0x5415, 
  0x5440, 0x5441, 0x5444, 0x5445, 0x5450, 0x5451, 0x5454, 0x5455, 
  0x5500, 0x5501, 0x5504, 0x5505, 0x5510, 0x5511, 0x5514, 0x5515, 
  0x5540, 0x5541, 0x5544, 0x5545, 0x5550, 0x5551, 0x5554, 0x5555
   ])

;;; fastest so far
(defn mbits [x y]
  (bit-or (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right y 24))) 49)
          (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right x 24))) 48)
          (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right y 16))) 33)
          (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right x 16))) 32)
          (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right y 8))) 17)
          (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right x 8))) 16)
          (bit-shift-left (morton256 (bit-and 0xFF y)) 1)
          (morton256 (bit-and 0xFF x))))


;;; seems like a good idea but it's slower
(defn update-mort-byte1 [z x right left]
  (bit-or z (bit-shift-left (morton256 (bit-and 0xFF (bit-shift-right x right))) left)))

(defn update-mort-byte [z x right left]
  (let [mb  (morton256 (bit-and 0xFF (bit-shift-right x right)))]
    (if (zero? mb)
      z
      (bit-or z (bit-shift-left mb left)))))

;;; much slower when refactored!  Probably not so good for JIT
(defn mbits2 [x y]
  (-> 0
      (update-mort-byte y 24 49)
      (update-mort-byte x 24 48)
      (update-mort-byte y 16 33)
      (update-mort-byte x 16 32)
      (update-mort-byte y 8 17)
      (update-mort-byte x 8 16)
      (update-mort-byte y 0 1)
      (update-mort-byte x 0 0)))

      
      





;;;; other stuff, not as good

;;; built a lookup table of 256 entries
(defn lut256 []
  (sort-by peek (for [x (range 16) y (range 16)]
                  (vector x y (bit-or (bit-shift-left y 4) x)
                          (zbits x y)))))


;;; new idea, add pre-shifted balue as well
;;; Not sure about offsets, needs thinking and testing
#_
(defn shifted-pairs [[yx ib]]
  (map #(vector (bit-shift-left yx (* % 4)) (bit-shift-left ib (* 8 %))) (range 8)))


;;; try basic map for [4bits y, 4 bits x] as int, mapped into interleaved bits
(def myxib (into {}
                 (for [x (range 16) y (range 16)]
                   (vector (bit-or (bit-shift-left y 4) x)
                           (zbits x y)))))

;;; THis should always be true as the interleaved bits should have the same count
#_
(assert (every? (fn [[a b]] (= (Long/bitCount a) (Long/bitCount b))) myxib))


;;; unsigned- shouldn't matter as x and y should be 32 bit unsigned ints, but not checked
(defn lutbits1 [x y]
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
;;; slightly faster than zbits but depends on data as zbits counts bits
(defn lutbits [x y]
  (let [y (bit-shift-left y 4)]
    (loop [off 28 z 0]
      (if (neg? off)
        z
        (let [yx (bit-or (bit-and 0xF0 (unsigned-bit-shift-right y off))
                         (bit-and 0xF (unsigned-bit-shift-right x off)))]
          (recur (- off 4)
                 (bit-or z (bit-shift-left (vyxib yx) (* 2 off)))))))))



;;; should consider byte-array ???  However, there's an issue with the bytes always being
;;; signed so you need to check sign and convert to appropriate int/long, which kind of
;;; complicates the code.  Not implemented.

;;; ----------------------------------------------------------------------

;;;; test and benchmark
(defn morton-test [mbits]
  (assert (= (mbits 0 0) 0))
  (assert (= (mbits 0 1) 2))
  (assert (= (mbits 1 1) 3))
  (assert (= (mbits 0XF0F17 0X3F0F171) 3002034673888023))
  (assert (= (mbits Integer/MAX_VALUE 0)  1537228672809129301))
  (assert (= (mbits 0 Integer/MAX_VALUE) 3074457345618258602))
  (assert (= (mbits Integer/MAX_VALUE Integer/MAX_VALUE) 4611686018427387903))
  true)


  
