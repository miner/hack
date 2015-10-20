;; http://rosettacode.org/wiki/Four_bit_adder

(ns miner.fourbit)

;; a bit is represented as a boolean (true/false)
;; a word is a big-endian vector of bits [true false true true] = 11

;; maybe little endian is more convenient???

(defn bvec
  "Return a bit vector representation of num with the given number of lowbits (default 32, the maximum)."
  ([num]
     {:pre [(<= 0 num 0xFFFFFFFF)]}
     (vec (drop-while false? (map #(bit-test num %) (range 31 -1 -1)))))
  ([num lowbits]
     (mapv #(bit-test num %) (range (dec lowbits) -1 -1))))

(defn bit4 [num]
  "Return a bit-vector representation of num using the low 4 bits."
  (bvec num 4))

(defn bvec->num [vb]
  "Covert a bit-vector into a normal number"
  (reduce (fn [tot bd] (+ (if bd 1 0) (* 2 tot))) 0 vb))

;; bits are represented by booleans (true or false)
;; bit vectors are big-endian (most significant bit first)
;; multiple values are returned as vectors

(defn or-gate [a b]
  (or a b))

(defn and-gate [a b]
  (and a b))

(defn not-gate [a]
  (not a))

(defn xor-gate [a b]
  (or-gate (and-gate (not-gate a) b) (and-gate a (not-gate b))))

(defn half-adder [a b]
  "result is [carry sum]"
  (let [carry (and-gate a b)
        sum (xor-gate a b)]
    [carry sum]))

(defn full-adder [a b c0]
  "result is [carry sum]"
  (let [[ca sa] (half-adder c0 a)
        [cb sb] (half-adder sa b)]
    [(or-gate ca cb) sb]))



(defn nbit-adder [va vb]
  "va and vb should be big endian bit vectors of the same size. The result
is a bit vector having one more bit (carry) than args."
  {:pre [(= (count va) (count vb))]}
  (let [[c sums] (reduce (fn [[carry sums] [a b]]
                              (let [[c s] (full-adder a b carry)]
                                [c (conj sums s)]))
                         ;; initial value: false carry and an empty list of sums
                         [false ()]
                         ;; rseq is constant-time reverse for vectors
                         (map vector (rseq va) (rseq vb)))]
    (vec (conj sums c))))


(defn four-bit-adder [a4 a3 a2 a1 b4 b3 b2 b1]
  "Returns [carry s4 s3 s2 s1]"
  (nbit-adder [a4 a3 a2 a1] [b4 b3 b2 b1]))
