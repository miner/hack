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


(defn bkpos2
  ([^long n] (if (neg? n)
               (bkpos2 (bit-clear n 63) 1)
               (bkpos2 n 0)))
  ([^long n ^long c]
   (loop [v n c c]
    (if (zero? v)
      c
      (recur (bit-and v (dec v)) (inc c))))))


;; this is slower -- only difference is no explicit loop.  Not sure if this is a real result
;; or just noise at this point.

(defn bkpos3 [^long n]
  (let [bkp (fn [^long v ^long c]
              (if (zero? v)
                c
                (recur (bit-and v (dec v)) (inc c))))]
    (if (neg? n)
      (bkp (bit-clear n 63) 1)
      (bkp n 0))))

(defn bkpos3a [^long n]
  (let [bkp (fn [^long v ^long c]
              (loop [v v c c]
               (if (zero? v)
                 c
                 (recur (bit-and v (dec v)) (inc c)))))]
    (if (neg? n)
      (bkp (bit-clear n 63) 1)
      (bkp n 0))))

;; slightly faster to bit-not than to clear one bit
(defn bkpos5 [^long n]
  (let [bkp (fn [^long v ^long c]
              (if (zero? v)
                c
                (recur (bit-and v (dec v)) (inc c))))]
    (if (neg? n)
      (- 64 (bkp (bit-not n) 0))
      (bkp n 0))))


(defn bkpos5a [^long n]
  (let [bkp (fn [^long v ^long c]
              (loop [v v c c]
                (if (zero? v)
                  c
                  (recur (bit-and v (dec v)) (inc c)))))]
    (if (neg? n)
      (- 64 (bkp (bit-not n) 0))
      (bkp n 0))))

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

;; the correct one to use
(defn bitc [^long n]
  (Long/bitCount n))


(defn bbcnt [^long n]
  (loop [c 0 n n]
    (if (zero? n)
      c
      ;; hard-wired for all the low byte cases
      (let [b (case (bit-and 0xFF n)
                0 0,

                (127 191 223 239 247 251 253 254) 7,

                (1 2 4 8 16 32 64 128) 1,

                (15 23 27 29 30 39 43 45 46 51 53 54 57 58 60 71 75 77 78 83 85 86 89 90 92
                    99 101 102 105 106 108 113 114 116 120 135 139 141 142 147 149 150 153
                    154 156 163 165 166 169 170 172 177 178 180 184 195 197 198 201 202 204
                    209 210 212 216 225 226 228 232 240) 4,

                (63 95 111 119 123 125 126 159 175 183 187 189 190 207 215 219 221 222 231
                    235 237 238 243 245 246 249 250 252) 6,

                (7 11 13 14 19 21 22 25 26 28 35 37 38 41 42 44 49 50 52 56 67 69 70 73 74
                   76 81 82 84 88 97 98 100 104 112 131 133 134 137 138 140 145 146 148 152
                   161 162 164 168 176 193 194 196 200 208 224) 3,

                (3 5 6 9 10 12 17 18 20 24 33 34 36 40 48 65 66 68 72 80 96 129 130 132 136
                   144 160 192) 2,

                (31 47 55 59 61 62 79 87 91 93 94 103 107 109 110 115 117 118 121 122 124
                    143 151 155 157 158 167 171 173 174 179 181 182 185 186 188 199 203 205
                    206 211 213 214 217 218 220 227 229 230 233 234 236 241 242 244 248) 5,

                255 8)]
        (recur (+ c b) (unsigned-bit-shift-right n 8))))))




(defn test-bitcount [f]
  (assert (every? #(= (Long/bitCount %) (f %))
                  (concat (range -10000 10000) (range (- Long/MAX_VALUE 10000) Long/MAX_VALUE))))
  true)



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
        
