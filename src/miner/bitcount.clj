(ns miner.bitcount)

;; just for fun, real code should just use Java inter-op
;; Integer/bitCount or Long/bitCount

;; https://en.wikipedia.org/wiki/Hamming_weight

;; https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan

;; Brian Kernighan's method
(defn bkpos32 [^long n]
  {:pre [(not (neg? n))]}
  (loop [v (int n) c (int 0)]
    (if (= v (int 0))
      c
      (recur (bit-and v (dec v)) (inc c)))))

;; originally used 64-bit unsigned so we handle the sign-bit specially
(defn bkpos [^long n]
  (loop [v (long (if (neg? n) (bit-clear n 63) n))
         c (long (if (neg? n) 1 0))]
    (if (= v (long 0))
      c
      (recur (bit-and v (dec v)) (inc c)))))


;; save this over hinted version
;; SEM: not sure all the casting is necessary
(defn bkpos-works [^long n]
  (let [bk (fn ^long [^long n]
             (loop [v (long n) c (long 0)]
               (if (= v (long 0))
                 c
                 (recur (bit-and v (dec v)) (inc c)))))]
    (if (neg? n)
      (- (long 64) (long (bk (bit-xor (long -1) (long n)))))
      (bk (long n)))))
      


(defn test-bitcount [f]
  (every? #(= (Integer/bitCount %) (f %)) (range 10000)))



;; "best method" for 32-bit
;; v = v - ((v >> 1) & 0x55555555);                    // reuse input as temporary
;; v = (v & 0x33333333) + ((v >> 2) & 0x33333333);     // temp
;; c = ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24; // count

;; for 64-bit some constants are different

;; Oops, I had the precedence of the & (bit-and) wrong when I first transcribed the algorithm
;; It's lower than + in C.

;; note original was unsigned v
(defn bitpop [n]
  (let [v (long n)
        v (unchecked-subtract v (bit-and (unsigned-bit-shift-right v 1) 0x5555555555555555))
        v (unchecked-add (bit-and v 0x3333333333333333)
                         (bit-and (unsigned-bit-shift-right v 2) 0x3333333333333333))]
    (bit-shift-right
     (unchecked-multiply (bit-and (+ v (unsigned-bit-shift-right v 4)) 0x0F0F0F0F0F0F0F0F)
                         0x0101010101010101)
     56)))
        
