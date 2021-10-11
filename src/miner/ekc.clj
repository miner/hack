(ns miner.ekc)


(def k-moves
  (into [] (for [r (range 8)
                 f (range 8)]
             (reduce (fn [b [rdiff fdiff]]
                       (let [r2 (+ r rdiff)
                             f2 (+ f fdiff)]
                         (if (and (>= r2 0) (>= f2 0) (<= r2 7) (<= f2 7))
                           (bit-set b (bit-or (bit-shift-left r2 3) f2))
                           b)))
                     0
                     [[-2 -1] [-1 -2] [-2 1] [-1 2] [1 2] [2 1] [1 -2] [2 -1]]))))


;; faster
(defn captures [board]
  (let [bfirst  (fn [n] (Long/numberOfTrailingZeros  n))
        bpop (fn [n] (bit-xor n (Long/lowestOneBit  n)))
        bseq (fn [n]
               (loop [n n bs ()]
                 (if (zero? n)
                   bs
                   (let [h (Long/highestOneBit  n)]
                     (recur (bit-xor n h) (conj bs (Long/numberOfTrailingZeros h)))))))
        coords (fn [k] (vector (unsigned-bit-shift-right k 3) (bit-and 7 k)))
        bkset (reduce-kv (fn [res r rank]
                           (reduce-kv (fn [res f x]
                                        (if (pos? x)
                                          (bit-set res (bit-or (bit-shift-left r 3) f))
                                          res))
                                      res
                                      rank))
                         0
                         board)]
    (transduce (take (dec (Long/bitCount bkset)))
               (fn ([res ks]
                    (let [k (bfirst ks)
                          kco (coords k)]
                      (into res
                            (map #(hash-set kco (coords %)))
                            (bseq (bit-and ks (k-moves k))))))
                 ([res] res))
               []
               (iterate bpop bkset))))






(defn captures3 [board]
  (let [bfirst  (fn [n] (Long/numberOfTrailingZeros  n))
        bpop (fn [n] (bit-and-not n (Long/lowestOneBit  n)))
        bseq (fn [n]
               (loop [n n bs ()]
                 (if (zero? n)
                   bs
                   (let [h (Long/highestOneBit  n)]
                     (recur (bit-and-not n h) (conj bs (Long/numberOfTrailingZeros h)))))))
        coords (fn [k] (vector (unsigned-bit-shift-right k 3) (bit-and 7 k)))
        bkset (transduce (comp cat (keep-indexed (fn [k b] (when (pos? b) k))))
                                   (completing bit-set)
                                   0
                                   board)]
    (transduce (take (dec (Long/bitCount bkset)))
               (fn ([res ks]
                    (let [k (bfirst ks)
                          kco (coords k)]
                      (into res
                            (map #(hash-set kco (coords %)))
                            (bseq (bit-and ks (k-moves k))))))
                 ([res] res))
               []
               (iterate bpop bkset))))


(defn captures2 [board]
  (let [bfirst  (fn [n] (Long/numberOfTrailingZeros  n))
        bpop (fn [n] (bit-and-not n (Long/lowestOneBit  n)))
        bseq (fn [n]
               (loop [n n bs ()]
                 (if (zero? n)
                   bs
                   (let [h (Long/highestOneBit  n)]
                     (recur (bit-and-not n h) (conj bs (Long/numberOfTrailingZeros h)))))))
        coords (fn [k] (vector (unsigned-bit-shift-right k 3) (bit-and 7 k)))]
    (transduce (take-while #(> (Long/bitCount  %) 1))
               (fn ([res ks]
                    (let [k (bfirst ks)
                          kco (coords k)]
                      (into res
                            (map #(hash-set kco (coords %)))
                            (bseq (bit-and ks (k-moves k))))))
                 ([res] res))
               []
               (iterate bpop
                        (reduce bit-set 0
                                (into [] (comp cat
                                               (keep-indexed (fn [k b] (when (pos? b) k))))
                                          board))))))




(defn set= [a b]
  (= (set a) (set b)))

(defn smoke-captures [captures]
  (assert (set= (captures [[0 0 0 1 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 1 0 0 0 1 0 0]
                        [0 0 0 0 1 0 1 0]
                        [0 1 0 0 0 1 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 1 0 0 0 0 0 1]
                        [0 0 0 0 1 0 0 0]])
             []))
  ;; no captures

  (assert (set= (captures [[0 0 0 1 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 1 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]])
             [#{[0 3] [2 4]}]))

  (assert (set= (captures [[0 0 0 1 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 1 0 0 0]
                        [0 0 1 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]
                        [0 0 0 0 0 0 0 0]])
             [#{[0 3] [2 4]} #{[2 4] [3 2]}]))
  true)
