;; http://rosettacode.org/wiki/Four_bit_adder

(ns miner.fourbit10 )

;; a bit is represented as an int 1 or 0
;; Clojure logic tests 0 as true so we have to compare as numbers.
;; a word is a big-endian vector of bits [1 0 1 1] = 11


;; bits are represented by int 0 or 1
;; bit vectors are big-endian (msb first)
;; multiple values are returned as vectors

;; int 1/0
(def b0 0)

(def b1 1)

(def b0? zero?)

(def b1? (complement b0?))

(defn bit [logical] (if logical b1 b0))


(comment  ;; true/false
(def b0 false)

(def b1 true)

(def b0? false?)

(def b1? true?)

(defn bit [logical] (if logical b1 b0))
)

(defn bvec
  ([num]
     {:pre [(<= 0 num 0xFFFFFFFF)]}
     (vec (drop-while b0? (map #(bit (bit-test num %)) (range 31 -1 -1)))))
  ([num lowbits]
     (mapv #(bit (bit-test num %)) (range (dec lowbits) -1 -1))))

(defn bit4 [num]
  (bvec num 4))

(defn bvec->num [vb]
  (reduce (fn [tot bd] (+ (if (b1? bd) 1 0) (* 2 tot))) 0 vb))

(defn or-gate [a b]
  (bit (or (b1? a) (b1? b))))

(defn and-gate [a b]
  (bit (and (b1? a) (b1? b))))

(defn not-gate [a]
  (bit (b0? a)))

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


;; big endian bit-vectors, results have one more bit (carry) than args
(defn recursive-nbit-adder
  ([va vb] (recursive-nbit-adder va vb () b0))
  ([va vb ls cin] 
     {:pre [(= (count va) (count vb))]}
     (if (seq va)
       (let [[c s] (full-adder (peek va) (peek vb) cin)]
         (recur (pop va) (pop vb) (conj ls s) c))
       (vec (conj ls cin)))))

;; about same speed using reduce
(defn nbit-adder [va vb]
  "va and vb should be big endian bit-vectors of the same size. The result is a bit vector having one more bit (carry) than args."
  {:pre [(= (count va) (count vb))]}
  (let [[c sumlist] (reduce (fn [[carry lsum] [a b]]
                              (let [[c s] (full-adder a b carry)]
                                [c (conj lsum s)]))
                            [b0 ()]
                            (map vector (rseq va) (rseq vb)))]
    (vec (conj sumlist c))))


(defn four-bit-adder [a4 a3 a2 a1 b4 b3 b2 b1]
  "Returns [carry s4 s3 s2 s1]"
  (nbit-adder [a4 a3 a2 a1] [b4 b3 b2 b1]))


(comment
  (= (+ 323 1002104) (bvec->num (nbit-adder (bvec 323 32) (bvec 1002104 32))))

)
